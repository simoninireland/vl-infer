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

;;; We use LISP-BINARY to read the flatbuffers file. There is one
;;; awkward aspect to this, which is that vtable pointers are signed
;;; and need to be /subtracted/ from the current position rather than
;;; /added/: we handle this with a custom type.
;;;
;;; We need to construct a binary structure for each object in the
;;; flatbuffers schema, and account for the offsets in an object's
;;; vtable that builders are at liberty to change. I have no idea
;;; why.... There's not quite enough type information stored in the
;;; flatbuffer, like with C++, so we need a slightly baroque manoeuvre
;;; to construct the necessary structures. We construct LISP-BINARY
;;; types for each flatbuffers type that needs it, constructing a type
;;; description record that can be used to translate the types, and
;;; ensure that the builder knows what type it is expecting when
;;; parsing the flatbuffer.
;;;
;;; It is also worth nothing that this implementation breaks-up the
;;; flatbuffer amongst the objects described, meaning that the
;;; "flatbuffer" isn't flat once it's been read. This might be an
;;; issue with large buffers.


;;; ---------- Flatbuffers to LISP-BINARY type mapping ----------

(defvar *expecting* nil
  "A stack of object types being read.

This is elaborated by the LISP-BINARY parser as the flatbuffer is read. The
top opf the stack holds the LISP-BINARY type description for the expected
object.")


(defvar *fb-type-map* (make-hash-table :test #'equal)
  "Map flatbuffers types to type metaobjects.

There is an entry in this mapping for every constructed type in
the schema: every array, table, struct, or enum.")


(defvar *lisp-binary-type-map* (make-hash-table)
  "Map LISP-BINARY representation type names types to type metaobjects.

There is an entry in this mapping for every constructed type in
the schema: every array, table, struct, or enum.")


(defvar *vtable-type-map* (make-hash-table)
  "Map the offset of a vtable to a type metaobject.")


(defun declare-lisp-binary-type (ty)
  "Add the type metaobject TY to the mappings."
  (setf (gethash (fb-type ty) *fb-type-map*) ty)
  (setf (gethash (representation ty) *lisp-binary-type-map*) ty))


(defun declare-lisp-binary-vtable-offset (ty offset)
  "Set the offset to the vtable associated with TY."
  (setf (gethash offset *vtable-type-map*) ty))


(defun get-type-metaobject-for-vtable-offset (offset)
  "Return the type metaobject associated with OFFSET."
  (gethash offset *vtable-type-map*))


(defun lisp-binary-type-metaobject (lbty)
  "Return the type metaobject corredponding to LBTY."
  (gethash lbty *lisp-binary-type-map* nil))


(defun get-type-metaobject-for-fb-type (fbty)
  "Return the type metaobjet for the flatbuffers type FBTYPE.

The type needs to be a table."
  (gethash fbty *fb-type-map*))


(defun fb-type-to-lisp-binary-type (fbty)
  "Return the LISP-BINARY type associated with FBTY."
  (or (fb-base-type-to-lisp-binary-type fbty)
      (gethash fbty *fb-type-map* nil)))


(defun fb-base-type-to-lisp-binary-type (fbty)
  "Return the LISP-BINARY type used to represent data of flatbuffers base type FBTY.

Return NIL if FBTY isn't a base type."
  (declare (optimize debug))

  (case (safe-car fbty)
    ;; base types
    (bool 'bit)
    ((byte ubyte int8 uint8) '(unsigned-byte 8))
    ((short ushort int16 uint16) '(unsigned-byte 16))
    ((int uint int32 uint32) '(unsigned-byte 32))
    ((long ulong int64 uint64) '(unsigned-byte 64))
    ((float float32) 'single-float)
    ((double float64) 'double-float)

    ;; strings are stored as separated counted string structures
    (string 'fb-string)

    (t nil)))


(defun fb-base-type-p (fbty)
  "Test whether flatbuffers type FBTY is a base type."
  (not (null (fb-base-type-to-lisp-binary-type fbty))))


(defun fb-array-type-element-type (fbty)
  "Return the element type of the flatbuffers array type FBTY."
  (and (listp fbty)
       (eql (safe-car fbty) 'array)
       (safe-cadr fbty)))


(defun fb-array-type-p (fnty)
  "Test whether flatbuffers type FBTY is an array type."
  (not (null (fb-array-type-element-type fbty))))


;;; ---------- Schema element type metaobjects ----------

(defgeneric lisp-binary-type-size (lbty)
  (:documentation "Returns how many bytes are needed to encode a value of LISP-BINARY type LBTY.")
  (:method (lbty)
    (cond
      ;; base types
      ((eql lbty 'bit)
       1)
      ((or (equal lbty '(unsigned-byte 8))
	   (equal lbty '(signed-byte 8)))
       1)
      ((or (equal lbty '(unsigned-byte 16))
	   (equal lbty '(signed-byte 16)))
       2)
      ((or (equal lbty '(unsigned-byte 32))
	   (equal lbty '(signed-byte 32)))
       4)
      ((eql lbty 'single-float)
       4)
      ((eql lbty 'double-float)
       8)

      ;; strings are stored as offsets
      ((eql lbty 'fb-string)
       4)

      (t
       (error "Unknown LISP-BINARY type ~a" lbty)))))


(defclass FB-Element ()
  ((name
    :initarg :name
    :reader name
    :documentation "The name of the element.")
   (fb-type
    :initarg :fb-type
    :initform nil
    :accessor fb-type
    :documentation "The flatbuffers type of this element.")
   (representation
    :initarg :representation
    :initform nil
    :accessor representation
    :documentation "The LISP-BINARY representation of this element.

This will either be a base type of LISP-BINARY (for example (UNSIGNED-BYTE 8))
of or the name of a structure created with DEFBINARY."))
  (:documentation "Base class for all flatbuffers elements."))


(defclass Field (FB-Element)
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


(defclass FB-Type (FB-Element)
  ()
  (:documentation "Base class for all flabuffer types."))


(defmethod initialize-instance :after ((ty FB-Type) &key &allow-other-keys)
  (declare-lisp-binary-type ty))


(defclass CArray (FB-Type)
  ((element-type
    :initarg :element-type
    :initform nil
    :reader element-type
    :documentation "The LISP-BINARY type for elements of the array.")))


(defmethod initialize-instance :after ((arr CArray) &key &allow-other-keys)
  (eval `(defbinary ,(representation arr) (:byte-order :little-endian)
	   (arr #1A() :type (counted-array 4 ,(element-type arr))))))


(defmethod lisp-binary-type-size ((arr CArray))
  8)


(defclass FB-Record (FB-Type)
  ((fields
    :initarg :fields
    :reader fields
    :documentation "A list of fields in canonical order."))
  (:documentation "Base class for all flatbuffers types that contain fields."))


(defclass Table (FB-Record)
  ())


(defmethod initialize-instance :after ((ty Table) &key &allow-other-keys)
  (declare (optimize debug))

  (flet ((create-field (struct field)
	   "Return the LISP-BINARY field entry for FIELD."
	   (if (deprecated-p field)
	       struct

	       (let* ((fbty (fb-type field))
		      (lbty (representation field))
		      (ty (lisp-binary-type-metaobject lbty))
		      fields)

		 ;; add the field
		 (cond
		   ;;strings
		   ((eq lbty 'fb-string)
		    (with-gensyms (base-pointer-name)
		      (appendf fields
			       (list `(,(name field) nil :type fb-string)))))

		   ;; base types
		   ((fb-base-type-p fbty)
		    (appendf fields (list `(,(name field) 0 :type ,lbty))))

		   ;; enums
		   ;; TBD

		   ;; tables
		   ((subtypep (type-of ty) 'Table)
		    (appendf fields
			     (list `(,(name field) nil :type (eval (progn
								     (push (get-type-metaobject-for-fb-type ',fbty) *expecting*)
								     'fb-table))))))

		   ;; structs
		   ((subtypep (type-of ty) 'Struct)
		    (appendf fields
			     (list `(,(name field) 0 :type ,lbty))))

		   ;; arrays
		   ((subtypep (type-of ty) 'CArray)
		    (with-gensyms (base-pointer-name)
		      (appendf fields
			       (list `(,(name field) nil :type ,lbty)))))

		   ;; that should cover everything
		   (t
		    (error "No representation for ~a" ty)))

		 (log:trace "Representing field ~a with ~a" (name field) struct)
		 (append struct fields)))))

    ;; create the fields
    (let* ((fields (foldr #'create-field (fields ty) nil))
	   (lbty (representation ty)))

      (log:debug "Creating ~a to represent fb type ~a" lbty (name ty))
      (eval `(defbinary ,lbty (:byte-order :little-endian)
	       ,@fields)))))


(defmethod lisp-binary-type-size ((ty Table))
  8)


(defclass Enumeration (FB-Record)
  ())


(defmethod lisp-binary-type-size ((ty Enumeration))
  (lisp-binary-type-size (representation ty)))


(defclass Struct (FB-Record)
  ())


(defmethod initialize-instance :after ((ty Struct) &key &allow-other-keys)
  (declare (optimize debug))

  (flet ((create-field (struct field)
	   "Return the code for FIELD at the given OFFSET."
	   ;; ignore any deprecated fields
	   (if (deprecated-p field)
	       struct

	       (let ((lbty (representation field)))

		 ;; add the fields
		 ;; ;TODO: Need to handle all field types
		 (cond
		   ;; numbers
		   ((subtypep lbty '(or unsigned-byte
				     signed-byte
				     float
				     double))
		    (appendf struct
			     (list `(,(name field) 0 :type ,lbty))))

		   (t
		    (error "Can't handle structure field for ~a" lbty)))))))

    (let ((fields (foldr #'create-field (fields ty) '()))
	  (lbty (representation ty)))

      (eval `(defbinary ,lbty (:byte-order :little-endian)
	       ,@fields)))))


(defmethod lisp-binary-type-size ((ty Struct))
  (foldr #'+ (mapcar (compose #'lisp-binary-type-size #'representation) (fields ty)) 0))


;;; ---------- Binary format ----------

(defmacro with-excursion (str there &body body)
  "Seek STR to THERE, perform BODY, and SEEK back to where we were."
  (with-gensyms (s here)
    `(let* ((,s ,str)
	    (,here (file-position ,s)))
       (file-position ,s ,there)
       (progn
	 ,@body)
       (file-position ,s ,here))))


(defbinary fb-header (:byte-order :little-endian)
  (root-object nil :type (pointer :pointer-type (unsigned-byte 32)
				  :data-type fb-table)))


(defbinary fb-vtable (:byte-order :little-endian)
  (vtable-base nil :type base-pointer)
  (vtable-length 0 :type (unsigned-byte 16))
  (table-length 0 :type (unsigned-byte 16))
  (field-offsets #1A() :type (eval `(simple-array (unsigned-byte 16) (,(/ (- vtable-length 4) 2))))))


(defbinary fb-table (:byte-order :little-endian)
  (table-base nil :type base-pointer)
  (vtable nil :type (custom :reader (lambda (str)
				       (declare (optimize debug))

				       (let* ((soffset (read-integer 4 str :signed t))
					      (there (- table-base soffset)))

					 (let ((vt (let ((here (file-position str)))
						     (file-position str there)
						     (prog1
							 (read-binary 'fb-vtable str)

						       (file-position str here)))))
					   (values vt 4))))
			     :lisp-type fb-vtable))
  (body nil :type (custom :reader (lambda (str)
				    (declare (optimize debug))

				    (let* ((ty (car *expecting*))
					   (fields (fields ty))
					   (offsets (fb-vtable-field-offsets vtable))
					   (v (make-instance (representation ty))))
				      (break)
				      ;; read the fields from the correct offsets
				      (dolist (i (iota (length offsets)))
					(let ((field (elt fields i))
					      (offset (elt offsets i)))
					  (unless (= offset 0)
					    (let ((v (let ((here (file-position str)))
						       (file-position str (+ table-base offset))
						       (prog1
							   (read-binary (representation field) str)

							 (file-position str here)))))
					      (break)
					      (value v 4)))))))
			  :lisp-type t)))


(defbinary fb-string (:byte-order :little-endian)
  (str "" :type (counted-string 4)))


;;; ---------- Builder functions ----------

;;; The approach here is to read in the schema as s-expressions, expressed in
;;; terms of flatbuffers types, and convert it into type metaobjects expressed
;;; in terms of LISP-BINARY types. The latter can be the built-in types for
;;; LISP-BINARY or the names of structures created wuth DEFBINARY.

(defparameter *defbinary-struct-counter* 0
  "Counter for building unique DEFBINARY struct names.")


(defun make-lisp-binary-type-name (stem)
  "Return a new symbol to name a binary structure defined with DEFBINARY.

STEM will be used as the first part of the name."
  (prog1
      (intern (upcase (concat stem "-" (format nil "~a" *defbinary-struct-counter*))))

    (incf *defbinary-struct-counter*)))


(defun make-field (name vars)
  "Make a metaobject for a field NAME over the given VARS.

This may involve making some auxiliary LISP-BINARY types to represent
the conmtents of the field."
  (declare (optimize debug))

  (let* ((fbty (getassoc :type vars))
	 (meta (safe-cdr (assoc :metadata vars)))
	 (deprecated-p (and (not (null meta))
			    (assoc 'deprecated meta)))
	 (lbty (or (fb-base-type-to-lisp-binary-type fbty)

		   ;; not a base type, check for constructed types
		   (if-let ((ty (fb-type-to-lisp-binary-type fbty)))
		     ;; we have a constructed type, use its representation
		     (representation ty)

		     ;; we don't have a type constructed yet
		     (if-let ((fbet (fb-array-type-element-type fbty)))
		       ;; we have an array, construct it
		       (let ((et (if (fb-base-type-p fbet)
				     ;; base type, use its translation
				     (fb-type-to-lisp-binary-type fbet)

				     ;; structure type, use its representation
				     (representation (fb-type-to-lisp-binary-type fbet))))
			     (rep (make-lisp-binary-type-name "fb-array")))

			 (make-instance 'CArray :fb-type fbty
						:element-type et
						:representation rep)
			 rep)

		       ;; not an array, any other type we reference should have
		       ;; been constructed alreay
		       (error "No type for ~a" fbty))))))

    (make-instance 'Field :name name
			  :fb-type fbty
			  :representation lbty
			  :deprecated (not (null deprecated-p)))))


(defun make-enum (name fbety elements)
  "Make a description for  an enumration NAME with flatbuffers element type FBETY and elements ELEMENTS."
  (let ((fields (car (foldr (lambda (fields-val f)
			      (destructuring-bind (field-name &rest vars)
				  f
				(destructuring-bind (fields val)
				    fields-val
				  (let ((v (or (getassoc :default vars)
					       val)))
				    (list (append fields
						  (list (make-instance 'Field :name field-name
									      :fb-type fbety
									      :value v)))
					  (1+ v))))))
			    elements (list '() 0)))))

    (make-instance 'Enumeration :name name
				:fb-type fbety
				:fields fields)))


(defun make-table (name elements)
  "Make a description for table NAME with fields built from ELEMENTS.

This may result in code to build additional types needed by the fields."
  (declare (optimize debug))
  (let ((lbty (make-lisp-binary-type-name "fb-table"))
	(fields (mapcar (lambda (f)
			 (destructuring-bind (field-name &rest vars)
			     f
			   (make-field field-name vars)))
		       elements)))

    (make-instance 'Table :name name
			  :fb-type name
			  :representation lbty
			  :fields fields)))


(defun make-struct (name elements)
  "Make a type metaobject for a structure NAME from the given ELEMENTS.

The code reated will create the LISP-BINARY type needed to represent the
structure as well."
  (declare (optimize debug))

  (let ((lbty (make-lisp-binary-type-name "fb-table"))
	(fields (mapcar (lambda (f)
			  (destructuring-bind (field-name &rest vars)
			     f
			    (make-field field-name vars)))
		       elements)))

    (make-instance 'Struct :name name
			   :fb-type name
			   :representation lbty
			   :fields fields)))


(defun make-object (object)
  "Make the type description for a schema OBJECT."
  (declare (optimize debug))

  (destructuring-bind (tag &rest args)
      object
    (case tag
      (enum
       (destructuring-bind (name fbety elements)
	   args
	 (make-enum name fbety elements)))

      (table
       (destructuring-bind (name elements)
	   args
	 (make-table name elements)))

      (struct
       (destructuring-bind (name elements)
	   args
	 (make-struct name elements)))

      (root-type
       nil)

      (t
       ;; warn for now
       (warn "No object constructed for ~s" tag)))))


(defun make-schema (schema)
  "Make the supporting structures for SCHEMA.

SCHEMA should be an S-expression-format schema definition, for example
as returned by PARSE-FBS-SCHEMA.

Return the type metaobject of the root type."
  (declare (optimize debug))

  (let ((root-type (fbs-root-type schema)))

    ;; empty the environment
    (setq *fb-type-map* (make-hash-table :test #'equal))
    (setq *lisp-binary-type-map* (make-hash-table))
    (setq *vtable-type-map* (make-hash-table))

    ;; create the necessary classes
    (mapc #'make-object schema)

    ;; return the description of the root type
    (get-type-metaobject-for-fb-type root-type)))


(defun read-fbs (fb root-type)
  "Parse a flatbuffers file starting with ROOT-TYPE.

FBS can be a pathname, a stream, or a string."
  (declare (optimize debug))

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
    (let ((*expecting* (list root-type)))
      (read-binary 'fb-header str))))
