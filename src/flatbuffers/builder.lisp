;;;; Flatbuffers builder
;;;;
;;;; Copyright (C) 2026 Simon Dobson
;;;;
;;;; This file is part of vl-infer, a machine learning inference accelerator builder
;;;;
;;;; vl-infer is free software: you can redistribute it and/or modify
;;;; it under the terms of the GNU General Public License as published by
;;;; the Free Software Foundation, either version 3 of the License, or
;;;; (at your option) any later version.
;;;;
;;;; vl-infer is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU General Public License
;;;; along with verilisp. If not, see <http://www.gnu.org/licenses/gpl.html>.

(in-package :vl-infer/flatbuffers)

;;; We use LISP-BINARY to read the flatbuffers file. There is one awkward
;;; aspect to this, which is that vtable pointers are signed and need to be
;;; /subtracted/ from the current position rather than /added/: we handle this
;;; with a custom type.
;;;
;;; We need to construct a binary structure for each object in the
;;; flatbuffers schema, and account for the offets in an object's vtable that
;;; builders are at liberty to change. I have no idea why.... There's not quite
;;; enough type information stored in the flatbuffer, like with C++, so we need
;;; a slightly baroque manoeuvre to construct the necessary structures.
;;;
;;; It is also worth nothing that this implementation breaks-up the flatbuffer
;;; amongst the objects described, meaning that the "flatbuffer" isn't flat
;;; once it's been read. This might be an issue with large buffers.


;;; ---------- Binary formats ----------

(defvar *expecting* nil
  "A stack of object types being read.")


(defvar *object-vtable* (make-hash-table)
  "A hash table from vtable offsets to type names.")


(defun object-for-vtable (offset)
  "Return the object type associated with the given vtable OFFSET.

This will be NIL the first time the vtable is encountered."
  (gethash offset *object-vtable* nil))


(defclass Table ()
  ((name
    :initarg :name
    :reader name
    :documentation "The name of the type.")
   (lisp-binary-type
    :initform nil
    :accessor lisp-binary-type
    :documentation "The name of the binary structure used to encode this object type.")
   (vtable
    :initform nil
    :accessor vtable
    :documentation "The object type's vtable structure.")
   (fields
    :initarg :fields
    :reader fields
    :documentation "The fields in canonical order.")))


(defclass Field ()
  ((name
    :initform (gensym)
    :initarg :name
    :reader name
    :documentation "The field name.")
   (lisp-binary-type
    :initarg :lisp-binary-type
    :reader lisp-binary-type
    :documentation "The LISP-BINARY type for this field.")
   (deprecated-p
    :initarg :deprecated
    :initform nil
    :reader deprecated-p
    :documentation "Flag for deprecation.")))


(defun create-lisp-binary-struct-fields (type offsets)
  "Create the code for a LISP-BINARY structure-class corresponding to TYPE with the given OFFSETS.

The struct may contain dummy fields to match the way the writer has
laid-out the data. Any deprecated fields are ignored.

Rweturn a list of binary structure field definitions."
  (declare (optimize debug))

  (let ((offset 4)) ; initial offset in the table after the vtable pointer

    (flet ((create-field (struct fieldesc)
	     "Return the code for FIELD at the given OFFSET."
	     (destructuring-bind (field field-offset)
		 fieldesc

	       ;; ignore any deprecated fields
	       (if (or (deprecated-p field)
		       (= field-offset 0))
		   struct

		   (let ((type (lisp-binary-type field))
			 fields)

		     ;; pad the structure if necessary
		     (let ((fill (- field-offset offset)))
		       (if (> fill 0)
			   (with-gensyms (dummy)
			     (appendf fields
				      (list `(,dummy #1A() :type (simple-array (unsigned-byte 8) (,fill))))))))

		     ;; add the fields
		     ;; ;TODO: Need to handle all field types
		     (cond
		       ;;strings
		       ((eq type 'string)
			(with-gensyms (base-pointer-name)
			  (appendf fields
				   (list `(,base-pointer-name nil :type base-pointer)
					 `(,(name field) nil :type (pointer :pointer-type (unsigned-byte 32)
									    :base-pointer-name ,base-pointer-name
									    :data-type fb-string))))
			  (incf offset 4)))

		       ;; numbers
		       ((subtypep type '(or (unsigned-byte 16)
					 (signed-byte 16)))
			(appendf fields
				 (list `(,(name field) 0 :type ,type)))
			(incf offset 2))
		       ((subtypep type '(or (unsigned-byte 32)
					 (signed-byte 32)))
			(appendf fields
				 (list `(,(name field) 0 :type ,type)))
			(incf offset 4))

		       ;; objects
		       (t
			(appendf fields
				 (list `(,(name field) nil :type (eval (progn
									 (push (lisp-binary-type field) *expecting*)
									 ,(lisp-binary-type field))))))
			(incf offset 4)))

		     (append struct fields))))))

      ;; offsets come in as an array but need to be a list
      (let (offsets-list)
	(dotimes (i (length offsets))
	  (appendf offsets-list (list (aref offsets i))))

	;; order the fields in offset-ascending order
	(let ((fields-offsets (zip (fields type) offsets-list)))
	  (sort fields-offsets #'< :key #'cadr)

	  (foldr #'create-field fields-offsets nil))))))


(defun make-lisp-binary-struct (type offsets)
  "Create the body structure for TYPE at the given OFFSETS."
  (declare (optimize debug))

  (let* ((fields (create-lisp-binary-struct-fields type offsets))
	 (lisp-binary-type (intern (upcase (concat "fb-table-fields-" (symbol-name (name type))))))
	 (struct-code `(defbinary ,lisp-binary-type (:byte-order :little-endian)
			 ,@fields)))
    (break)
    ;; build the structure
    (eval struct-code)

    ;; record its name for later reference
    (setf (lisp-binary-type type) lisp-binary-type)))


(defbinary fb-header (:byte-order :little-endian)
  (root-object nil :type (pointer :pointer-type (unsigned-byte 32)
				  :data-type fb-table-header)))


(defbinary fb-vtable (:byte-order :little-endian)
  (vtable-base nil :type base-pointer)
  (vtable-length 0 :type (unsigned-byte 16))
  (table-length 0 :type (unsigned-byte 16))
  (field-offsets nil :type (eval `(simple-array (unsigned-byte 16) (,(/ (- vtable-length 4) 2)))))
  (vtable-index 0 :type (custom :reader (lambda (str)
				    (let ((object-type (car *expecting*)))
				      (make-lisp-binary-struct object-type field-offsets)
				      (values 1 0)))
			  :lisp-type (unsigned-byte 16))))


(defbinary fb-table-header (:byte-order :little-endian)
  (vtable nil :type (custom :reader (lambda (str)
				      (declare (optimize debug))
				      (let* ((here (file-position str))
					     (soffset (read-integer 4 str :signed t))
					     (there (- here soffset))
					     (after (file-position str))
					     (object (object-for-vtable there))
					     (vtable (if object
							 ;; we've seen this object type before,
							 ;; return its vtable struct
							 (vtable object)

							 ;; we haven't seen this object type before
							 (progn
							   ;; read the vtable -- this creates the
							   ;; body type too
							   (file-position str there)
							   (setf vtable (read-binary 'fb-vtable str))
							   (file-position str after)

							   ;; add it to the table
							   (let ((object (car *expecting*)))
							     (setf (vtable object) vtable))

							   vtable))))

					(values vtable 4)))
			    :lisp-type fb-vtable))
  (body nil :type (eval (let ((object-type (car *expecting*)))
			  (lisp-binary-type object-type)))))


(defbinary fb-string (:byte-order :little-endian)
  (str "" :type (counted-string 4)))






;; ;;; ---------- Buffer builder ----------

;; (defclass Builder ()
;;   ((buffer
;;     :documentation "The underlying flatbuffer."
;;     :initarg :buffer
;;     :initform nil
;;     :reader buffer)
;;    (offset
;;     :documentation "Current offset for writing into the buffer."
;;     :initform 0
;;     :accessor offset)
;;    (vtables
;;     :documentation "Hash table of vtable offsets."
;;     :initform (make-hash-table))
;;    (strings
;;     :documentation "Hash table of string offsets and references."
;;     :initform (make-hash-table :test #'equal)))
;;   (:documentation "Builder for a flatbuffer."))


;; (defmethod initialize-instance :after ((builder Builder) &key &allow-other-keys)
;;   (with-slots (buffer offset file-identifier)
;;       builder

;;     ;; normalise the file identifier to four characters
;;     (let ((n (length file-identifier)))
;;       (cond ((< n 4)
;;	     (setf file-identifier (concat file-identifier (make-string (- 4 n) :initial-element (code-char 0)))))
;;	    ((> n 4)
;;	     (setf file-identifier (substring 0 4 file-identifier)))))

;;     (if buffer
;;	;; we have an underlying buffer, set the offset to that of the root object
;;	(select-root-object builder)

;;	;; no buffer, create one
;;	(progn
;;	  (setf buffer (make-array '(1024)
;;				   :element-type '(unsigned-byte 8)
;;				   :adjustable t))

;;	  ;; initialise the buffer
;;	  (let ((root-offset #16r00000100))
;;	    ;; pointer to root object
;;	    (store-integer root-offset builder)

;;	    ;; set the offset to point to the root object
;;	    (setf offset root-offset))))))


;; (defun root-object-offset (builder)
;;   "Return the offset to the root object of BUILDER."
;;   (retrieve-integer builder 0))


;; (defun select-root-object (builder)
;;   "Set the offset of BUILDER to point to the root object."
;;   (setf (offset builder) (root-object-offset builder)))


;; (defclass OffsetRefs ()
;;   ((offset
;;     :documentation "The offset of the object within the flatbuffer."
;;     :initform nil
;;     :accessor offset)
;;    (references
;;     :documentation "The offsets of references to the object."
;;     :initform nil
;;     :accessor references))
;;   (:documentation "An object being stored in a flatbuffer.

;; Objects of this class are intended as members ot the strings and vtables
;; hash tables. They initially store the offsets of references to the
;; objects; when the table is written these offsets are patched to the actual
;; offset to the object."))


;; (defun store-strings-table (builder)
;;   "Write the strings table of BUILDER.

;; This writes the strings and patches the offsets that refer to them. This
;; should be the last thing that happens when building the flatbuffer, since
;; strings need to come at the end."
;;   (flet ((store-string (k v)
;;	   "Write string K to the strings table."
;;	   (with-slots (offset)
;;	       v

;;	     ;; grab the offset of the string
;;	     (setf offset (offset builder))

;;	     ;; write the string into the string table
;;	     (let ((n (length k)))
;;	       ;; write the length
;;	       (store-integer n builder)

;;	       ;; write the elements of the string as bytes
;;	       (store-string-as-bytes k builder))))

;;	 (patch-string (k v)
;;	   "Patch references to string K to its offset."
;;	   (declare (ignore k))

;;	   (with-slots (offset references)
;;	       v

;;	     ;; patch each reference
;;	     (dolist (ref references)
;;	       (let ((string-offset (- offset ref)))
;;		 (store-integer string-offset builder ref))))))

;;     ;; write the string table
;;     (with-slots (strings)
;;	builder

;;       ;; write the strings
;;       (maphash #'store-string strings)

;;       ;; patch all uses of the strings in the buffer
;;       (maphash #'patch-string strings))))


;; (defun store-vtables-table (builder)
;;   "Write the vtables table of BUILDER.

;; This writes the strings and patches the offsets that refer to them.
;; This should be the next-to-last thing that happens when building the
;; flatbuffer, since strings need to come near the end before strings."
;;   (flet ((store-vtable (k v)
;;	   "Write the vtable K to the vtables table."


;;	   )))

;;   (with-slots (vtables)
;;       builder

;;     ;; write the vtables
;;     (maphash #'store-vtable vtables)

;;     ;; patch the references
;;     (maphash #'patch-vtable vtables)

;;     )
;;   )

;; ;;; ---------- Writing to the buffer ----------

;; (defun store-byte (b builder &optional offset)
;;   "Write byte B to BUILDER.

;; If OFFSET is omitted the byte is written to the current offset of
;; BUILDER and the offset is incremented by one."
;;   (let ((i (or offset
;;	       (offset builder))))
;;     (setf (aref (buffer builder) i) b)
;;     (unless offset
;;       (incf (offset builder)))))


;; (defun store-bytes (bs builder &optional offset)
;;   "Write the array of bytes BS to BUILDER.

;; If OFFSET is omitted the bytes are written to the current offset of
;; BUILDER and the offset is incremented by the number of bytes written."
;;   (dotimes (i (length bs))
;;     (store-byte (elt bs i) builder offset)
;;     (when offset
;;       (incf offset))))


;; (defun store-short (n builder &optional offset)
;;   "Write N as a 16-bit short to BUILDER.

;; If OFFSET is omitted the short is written to the current offset of
;; BUILDER and the offset is incremented by two."
;;   (store-bytes (16-bits-little-endian n) builder offset))


;; (defun store-integer (n builder &optional offset)
;;   "Write N as a 32-bit long to BUILDER.

;; If OFFSET is omitted the short is written to the current offset of
;; BUILDER and the offset is incremented by four."
;;   (store-bytes (32-bits-little-endian n) builder offset))


;; (defun store-long (n builder &optional offset)
;;   "Write N as a 64-bit long integer to BUILDER.

;; If OFFSET is omitted the short is written to the current offset of
;; BUILDER and the offset is incremented by eight."
;;   (store-bytes (64-bits-little-endian n) builder offset))


;; (defun store-offset (ptr builder &optional offset)
;;   "Write a 32-bit PTR to BUILDER.

;; PTR is treated as an unsigned offset.

;; If OFFSET is omitted the pointer is written to the current offset of
;; BUILDER and the offset is incremented by four."
;;   (store-bytes (32-bits-little-endian ptr) builder offset))


;; (defun store-string (s builder &optional offset)
;;   "Write S to BUILDER.

;; S is not actually written: instead it is entered into the strings table
;; of BUILDER to be written out at the end of the flatbuffer.

;; If OFFSET is omitted the pointer is written to the current offset of
;; BUILDER."
;;   (with-slots (strings)
;;       builder

;;     (multiple-value-bind (v present-p)
;;	(gethash s strings)

;;       (unless present-p
;;	;; string is not in the table, make an entry
;;	(setf v (make-instance 'OffsetRefs))
;;	(setf (gethash s strings) v))

;;       ;; record a reference to the string
;;       (let ((i (or offset
;;		   (offset builder))))
;;	(appendf (references v) (list i)))

;;       ;; write a placeholder for the pointer
;;       (store-integer 0 builder offset))))


;; (defun store-string-as-bytes (s builder &optional offset)
;;   "Store S as a string of bytes into BUILDER.

;; If OFFSET is omitted the string is written to the current offset of
;; BUILDER and the offset is incremented by the length of the string.
;; The string is always 16-bit aligned."
;;   (let* ((n (length s)))
;;     (dotimes (i n)
;;       (store-byte (char-code (aref s i)) builder))

;;     ;; pad to 16 bits if needed
;;     (when (oddp n)
;;       (store-byte 0 builder))))


;; (defun store-vtable (vtable builder)
;;   "Store VTABLE as a vtable in BUILDER.

;; VTABLE is not actually written: instead it is entered into the vtables table
;; of BUILDER to be written out at the end of the flatbuffer."
;;   (with-slots (vtables)
;;       builder

;;     (multiple-value-bind (v present-p)
;;	(gethash vtables))

;;     )


;;   )

;; ;;; ---------- Reading from the buffer ----------

;; (defun retrieve-byte (builder &optional offset)
;;   "Read a byte from BUILDER.

;; If OFFSET is omitted the byte is read from the current offset of
;; BUILDER and the offset is incremented by one."
;;   (let* ((i (or offset
;;		(offset builder))))
;;     (prog1
;;	;; return the byte
;;	(aref (buffer builder) i)

;;       (unless offset
;;	;; increment the offset in the builder
;;	(incf (offset builder))))))


;; (defun retrieve-bytes (n builder &optional offset)
;;   "Read N bytes from BUILDER as an array.

;; If OFFSET is omitted the byte is read from the current offset of
;; BUILDER and the offset is incremented by N."
;;   (prog1
;;       ;; return an array displaced into the buffer to avoid copying
;;       (make-array (list n)
;;		  :element-type '(unsigned-byte 8)
;;		  :displaced-to (buffer builder)
;;		  :displaced-index-offset (or offset
;;					      (offset builder)))

;;     (unless offset
;;       ;; increment the offset in the builder
;;       (incf (offset builder) n))))


;; (defun retrieve-short (builder &optional offset)
;;   "Read a 16-bit short from BUILDER.

;; If OFFSET is omitted the byte is read from the current offset of
;; BUILDER and the offset is incremented by two."
;;   (from-little-endian (retrieve-bytes 2 builder offset)))


;; (defun retrieve-integer (builder &optional offset)
;;   "Read a 32-bit integer from BUILDER.

;; If OFFSET is omitted the byte is read from the current offset of
;; BUILDER and the offset is incremented by four."
;;   (from-little-endian (retrieve-bytes 4 builder offset)))


;; (defun retrieve-long (builder &optional offset)
;;   "Read a 64-bit long integer from BUILDER.

;; If OFFSET is omitted the byte is read from the current offset of
;; BUILDER and the offset is incremented by eight."
;;   (from-little-endian (retrieve-bytes 8 builder offset)))


;; (defun retrieve-string (builder &optional offset)
;;   "Read a string from BUILDER.

;; If OFFSET is omitted the byte is read from the current offset of
;; BUILDER and the offset is incremented by four, which is the length
;; of the offset to the string (/not/ the length of the string itself)."
;;   (with-slots (buffer)
;;       builder

;;     (let* ((here (or offset
;;		     (offset builder)))
;;	   (string-offset (+ here (retrieve-integer builder offset)))
;;	   (string-length (retrieve-integer builder string-offset))
;;	   (string-contents (+ string-offset 4))
;;	   (s (make-string string-length)))

;;       (dotimes (i string-length)
;;	(setf (aref s i) (code-char (aref buffer (+ string-contents i)))))

;;       s)))


;; (defun retrieve-string-as-bytes (n builder &optional offset)
;;   "Retrieve a string of N bytes from BUILDER.

;; If OFFSET is omitted the byte is read from the current offset of
;; BUILDER and the offset is incremented by N."
;;   (let ((s (make-string n)))
;;     (dotimes (i n)
;;       (setf (aref s i) (code-char (retrieve-byte builder offset)))
;;       (when offset
;;	(incf offset)))

;;     s))
