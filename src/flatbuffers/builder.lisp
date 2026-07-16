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


;;; ---------- Schema elements ----------

(defvar *expecting* nil
  "A stack of object types being read.")


(defvar *object-type-vtable* (make-hash-table)
  "A hash table from vtable offsets to types.")


(defun object-type-for-offset (offset)
  "Return the object type associated with the given vtable OFFSET.

This will be NIL the first time the vtable is encountered."
  (gethash offset *object-type-vtable* nil))


(defclass Flatbuffers-Object ()
  ((name
    :initarg :name
    :reader name
    :documentation "The name of the type.")
   (lisp-binary-type
    :initarg :lisp-binary-type
    :initform nil
    :accessor lisp-binary-type
    :documentation "The name of the binary structure used to encode this object type."))
  (:documentation "Base class for all flatbuffers objects.

These onbjects are used to describe structures present in flatbuffers
and their schemata."))


(defclass Table (Flatbuffers-Object)
  ((vtable
    :initform nil
    :accessor vtable
    :documentation "The object type's vtable structure.")
   (fields
    :initarg :fields
    :reader fields
    :documentation "A list of fields in canonical order.")))


(defclass Field (Flatbuffers-Object)
  ((value
    :initarg :value
    :initform nil
    :reader value
    :documentation "The default value for this field.")
   (deprecated-p
    :initarg :deprecated
    :initform nil
    :reader deprecated-p
    :documentation "Flag for deprecation.")))


(defclass Enumeration (Flatbuffers-Object)
  ((fields
    :initarg :fields
    :reader fields
    :documentation "A list of (field value) lists in canonical order.")))


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
									 (push ',type *expecting*)
									 ,type)))))
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


(defun create-lisp-binary-struct (type lisp-binary-type offsets)
  "Create the code for the binary structure representing TYPE with OFFSETS.

LISP-BINARY-TYPE should be the symbol naming the binmary structure created."
  (declare (optimize debug))

  (let* ((fields (create-lisp-binary-struct-fields type offsets)))

    `(defbinary ,lisp-binary-type (:byte-order :little-endian)
       ,@fields)))


(defun make-lisp-binary-struct (type offsets)
  "Create the body structure for TYPE at the given OFFSETS."
  (declare (optimize debug))

  (let* ((lisp-binary-type (intern (upcase (concat "fb-table-field-" (symbol-name (name type))))))
	 (struct-code (create-lisp-binary-struct type lisp-binary-type offsets)))

    ;; build the structure
    ;; We mask SIMPLE-ERROR conditions because they're signalled when a
    ;; struct is re-defined (in SBCL, anyway) and we want to be able to
    ;; do so.
    (eval `(handler-case
	       ,struct-code
	     (simple-error (e)
	       (continue e))))

    ;; record its name for later reference
    (setf (lisp-binary-type type) lisp-binary-type)))


;;; ---------- Binary format ----------

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
					     (cl (object-type-for-offset there))
					     (vtable (if cl
							 ;; we've seen this object type before,
							 ;; return its vtable struct
							 (vtable cl)

							 ;; we haven't seen this object type before
							 (progn
							   ;; read the vtable -- this creates the
							   ;; body type too
							   (file-position str there)
							   (setf vtable (read-binary 'fb-vtable str))
							   (file-position str after)

							   ;; add it to the table
							   (let ((cl (car *expecting*)))
							     (setf (vtable cl) vtable))

							   vtable))))

					(values vtable 4)))
			    :lisp-type fb-vtable))
  (body nil :type (eval (let ((object-type (car *expecting*)))
			  (lisp-binary-type object-type)))))


(defbinary fb-string (:byte-order :little-endian)
  (str "" :type (counted-string 4)))


;;; ---------- Builder functions ----------

(defun fb-to-lisp-binary-type (ty)
  "Return the LISP-BINARY type used to represent data of flatbuffers type TY."
  (declare (optimize debug))

   (case ty
    ;; base types
    (bool 'bit)
    ((byte ubyte int8 uint8) '(unsigned-byte 8))
    ((short ushort int16 uint16) '(unsigned-byte 16))
    ((int uint int32 uint32) '(unsigned-byte 32))
    ((long ulong int64 uint64) '(unsigned-byte 64))
    ((float float32) 'single-float)
    ((double float64) 'double-float)
    (string 'fb-string)

    ;; union, object, and structure types
    (t
     ty)))


(defun fb-type-offset (ty)
  "Return the number of bytes reqired for flatbuffer type TY.

This is used as the offset into the flatbuffer structure."
  (declare (optimize debug))

  (case ty
    ;; base types
    (bool 1)
    ((byte ubyte int8 uint8) 1)
    ((short ushort int16 uint16) 2)
    ((int uint int32 uint32) 4)
    ((long ulong int64 uint64) 8)
    ((float float32) 4)
    ((double float64) 8)
    (string 4)

    ;; union, object, and structure types
    (t
     (if (flatbuffers-object-p ty)
	 4

	 (error "Unrecognised flatbuffers type ~s" ty)))))


;;TODO: Move to utils

(defun safe-cadr (l)
  "Return the CADR of L if possible, otherwise NIL."
  (when (and (listp l)
	     (>= (length l) 2))
    (cadr l)))


(defun getassoc (item alist &key default)
  "Return the value associated to ITEM in ALIST.

DEFAULT will be returned if no association exists, and itself defaults
to NIL."
  (if-let ((a (assoc item alist)))
    (safe-cadr a)
    default))


(defun create-object (object)
  "Create the code for a schema OBJECT."
  (declare (optimize debug))

  (destructuring-bind (tag &rest args)
      object
    (let (root-object)

      (case tag
	(enum
	 (destructuring-bind (name ty vars)
	     args

	   (let* ((lisp-binary-type (fb-to-lisp-binary-type ty))
		  (fields (car (foldr (lambda (fields-val f)
					(destructuring-bind (field-name &rest vars)
					    f
					  (destructuring-bind (fields val)
					      fields-val
					    (let ((v (or (getassoc :default vars)
							 val)))
					      (list (append fields
							    (list (make-instance 'Field :name field-name
											:lisp-binary-type lisp-binary-type
											:value v)))
						    (1+ v))))))
				      vars (list '() 0)))))

	     `(defclass ,name (Enumeration)
		()
		(:default-initargs :name ',name
				   :lisp-binary-type ',lisp-binary-type
				   :fields ',fields)))))

	(table
	 (destructuring-bind (name vars)
	     args

	   (let ((fields (foldr (lambda (fields f)
				  (destructuring-bind (field-name &rest vars)
				      f
				    (let* ((fb-ty (getassoc :type vars))
					   (lisp-binary-type (fb-to-lisp-binary-type fb-ty)))
				      (append fields
					      (list (make-instance 'Field :name field-name
									  :lisp-binary-type lisp-binary-type))))))
				vars '())))

	     `(defclass ,name (Table)
		()
		(:default-initargs  :name ',name
				    :fields ',fields)))))

	(root-type
	 nil)

	(t
	 ;; warn for now
	 (warn "No object constructed for ~s" tag))))))


(defun create-schema (schema)
  "Create the code for SCHEMA.

SCHEMA should be an S-expression-format schema definition, for example
as returned by PARSE-FBS-SCHEMA. The code returned will define the
supporting types and return the root type of the schema, which will be
a TABLE object."
  (declare (optimize debug))

  (let* ((objects-code (remove-if #'null (mapcar #'create-object schema)))
	 (root-type (fbs-root-type schema)))

    `(progn
       ,@objects-code)))


(defun make-schema (schema)
  "Make the supporting structures for SCHEMA.

Return the class of the root type."
  (let ((schema-code (create-schema schema))
	(root-type (fbs-root-type schema)))

    ;; create the necessary classes
    (eval schema-code)

    ;; return the class of the root type
    (find-class root-type)))


(defun read-fbs (fb root-type)
  "Parse a flatbuffers file starting with ROOT-TYPE.

FBS can be a pathname, a stream, or a string."
  (let (str)
    (cond ((pathnamep fb)
	   ;; pathname, read from a file
	   (setq str (open fb :direction :input :element-type '(unsigned-byte 8))))

	  ((streamp fb)
	   ;; stream, read from it
	   (setq str fb))

	  ((stringp fb)
	   ;; string, use literally
	   (setq str (make-string-input-stream fb)))

	  (t
	   (error "Can't parse flatbuffer from ~a" fb)))

    ;; read and parse the stream
    (let ((*expecting* (list (make-instance root-type)))
	  (*object-type-vtable* (make-hash-table)))
      (read-binary 'fb-header str))))
