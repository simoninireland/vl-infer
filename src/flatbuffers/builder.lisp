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
