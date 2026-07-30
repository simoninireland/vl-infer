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


;;; ---------- Excursions within open streams ----------

;;; Macros for navigating the pointer structure of flatbuffers.

(defmacro with-excursion ((str there) &body body)
  "Seek STR to THERE, perform BODY, and SEEK back to where we were.

STR should be a variable holding a stream, and will be available in BODY."
  (with-gensyms (here)
    `(let ((,here (file-position ,str)))
       (file-position ,str ,there)
       (prog1
	   (progn
	     ,@body)
	 (file-position ,str ,here)))))


(defmacro with-pointer-excursion ((str) &body body)
  "Read a pointer from STR and there.

STR should be a variable holding a stream, and will be available in BODY.
The pointer should be a 32-bit integer based at the current position."
  (with-gensyms (here ptr)
    `(let ((,here (file-position ,str))
	   (,ptr (read-integer 4 ,str)))
       (with-excursion (,str (+ ,here ,ptr))
	 ,@body))))


;;; ---------- Flatbuffers to LISP-BINARY type mapping ----------

(defvar *expecting* nil
  "A stack of object types being read.

This is elaborated by the LISP-BINARY parser as the flatbuffer is read. The
top opf the stack holds the LISP-BINARY type description for the expected
object.")


(defmacro with-expecting (cl &body body)
  "Evaluate BODY in an environment where we're expecting a CL."
  `(let ((*expecting* (cons ,cl *expecting*)))
     ,@body))


(defvar *fb-type-map* (make-hash-table :test #'equal)
  "Map flatbuffers types to type metaobjects.

There is an entry in this mapping for every constructed type in
the schema: every array, table, struct, or enum.")


(defvar *lisp-binary-type-map* (make-hash-table)
  "Map LISP-BINARY representation type names types to type metaobjects.

There is an entry in this mapping for every constructed (non-base)
type in the schema: every array, table, struct, or enum.")


(defun declare-fb-complex-type (ty)
  "Declare TY as the metaobject for a complex flatbuffers type.

The type metaobject is mapped to its flatbuffers type and its name."
  (setf (gethash (fb-type ty) *fb-type-map*) ty)
  (setf (gethash (representation ty) *lisp-binary-type-map*) ty))


(defun fb-base-type-to-lisp-binary-type (fbty)
  "Return the LISP-BINARY type used to represent data of flatbuffers base type FBTY.

Return NIL if FBTY isn't a base type."
  (declare (optimize debug))

  (case (safe-car fbty)
    ;; base types
    (bool '(unsigned-byte 8))
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


(defun fb-complex-type-to-type (fbty)
  "Return the type metaobject associated with FBTY."
  (gethash fbty *fb-type-map* nil))


(defun complex-type-to-type (rep)
  "Return the type metaobject associated with REP."
  (gethash rep *lisp-binary-type-map* nil))


(defun fb-complex-type-p (fbty)
  "Test whether flatbuffers type FBTY is a complex type.

Complex types are enumerations, structures, and tables."
  (not (null (fb-complex-type-to-type fbty))))


(defun fb-array-type-p (fbty)
  "Test whether flatbuffers type FBTY is an array type."
  (and (listp fbty)
       (eql (safe-car fbty) 'array)))


(defun fb-array-type-element-type (fbty)
  "Return the element type of FBTY, if it is an arrayp

This will be another flatbuffers type, either base or complex
(but not another array type)."
  (and (fb-array-type-p fbty)
       (safe-cadr fbty)))


(defun lisp-binary-type-for-fb-type (fbty)
  "Return the LISP-BINARY type representing FBTYPE."
  (or (fb-base-type-to-lisp-binary-type fbty)
      (if-let ((ty (fb-complex-type-to-type fbty)))
	(representation ty))))


;;; ---------- Binary format ----------

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

					(let ((vt (with-excursion (str there)
						    (read-binary 'fb-vtable str))))
					  (values vt 4))))
			    :lisp-type t))
  (body nil :type (custom :reader (lambda (str)
				    (declare (optimize debug))

				    (let* ((ty (car *expecting*))
					   (fields (fields ty))
					   (offsets (fb-vtable-field-offsets vtable))
					   (constructor (symbol-function (intern (upcase (concat "make-" (symbol-name (representation ty)))))))
					   kvs)

				      ;; read the fields from the correct offsets
				      (dolist (i (iota (length offsets)))
					(let* ((field (elt fields i))
					       (offset (elt offsets i))
					       (fty (type-of (field-type field)))
					       (k (make-keyword (name field))))

					  (unless (= offset 0)
					    (let ((f (with-excursion (str (+ table-base offset))
						       (cond ((subtypep fty 'Struct)
							      ;; structs appear inline
							       (read-binary (representation field) str))

							     ((eql (representation field) 'fb-string)
							      ;; strings are indirected
							       (with-pointer-excursion (str)
								 (read-binary 'fb-string str)))

							     ((subtypep fty 'CArray)
							      ;; arrays may need to update the expected type
							       (with-pointer-excursion (str)
								 (read-binary (representation field) str)))

							     ((subtypep fty 'Table)
							      ;; tables read an fb-table
							       (with-expecting (field-type field)
								 (with-pointer-excursion (str)
								   (read-binary 'fb-table str))))

							     (t
							      ;; evertything else is inline
							       (read-binary-type (representation field) str))))))

					      (appendf kvs (list k f))))))

				      (let ((v (apply constructor kvs)))
					(values v (fb-vtable-table-length vtable)))))
			  :lisp-type t)))


(defbinary fb-string (:byte-order :little-endian)
  (str "" :type (counted-string 4)))


(defbinary fb-table-array  (:byte-order :little-endian)
  (arr #1A() :type )

  )

;;; ---------- Schema element type metaobjects ----------

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
    :documentation "Flag for deprecation.")
   (type
    :initarg :type
    :initform nil
    :reader field-type
    :documentation "The type metaobject describing this field.

This will be NIL if the field holds a base type.")))


(defclass FB-Type (FB-Element)
  ()
  (:documentation "Base class for all flabuffer types."))


(defmethod initialize-instance :after ((ty FB-Type) &key &allow-other-keys)
  ;; record the type metaobject
  (declare-fb-complex-type ty))


(defclass CArray (FB-Type)
  ((element-type
    :initarg :element-type
    :initform nil
    :reader element-type
    :documentation "The LISP-BINARY type for elements of the array.")))


(defmethod initialize-instance :after ((arr CArray) &key &allow-other-keys)
  (declare (optimize debug))

  (let* ((et (element-type arr))
	 (ety (if-let ((cl (complex-type-to-type et)))
		(if (subtypep (type-of cl) 'Table)
		    `(custom :reader (lambda (str)
				       (with-expecting ,cl
					 (let ((arr (make-array (list count))))
					   (dotimes (i count)
					     (let ((here (file-position str))
						   (ptr (read-integer 4 str)))

					       (with-excursion (str (+ here ptr))
						 (setf (aref arr i) (read-binary 'fb-table str)))))

					   (values arr (* count 4))))))

		    ;; other types are inline and can be read simply
		    `(simple-array ,et (count)))
		`(simple-array ,et (count)))))

    (eval `(defbinary ,(representation arr) (:byte-order :little-endian)
	     (array-base 0 :type base-pointer)
	     (count 0 :type (unsigned-byte 32))
	     (arr #1A() :type ,ety)))))


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

  (flet ((create-field (field)
	   "Return the LISP-BINARY field entry for FIELD."
	   (if (deprecated-p field)
	       nil

	       (let* ((fbty (fb-type field))
		      (lbty (representation field))
		      (ty (and (fb-complex-type-p fbty)
			       (fb-complex-type-to-type fbty))))

		 ;; add the field
		 (cond
		   ;;strings
		   ((eq lbty 'fb-string)
		    `(,(name field) 0 :type fb-string))

		   ;; base types
		   ((fb-base-type-p fbty)
		    `(,(name field) 0 :type ,lbty))

		   ;; enums
		   ((subtypep (type-of ty) 'Enumeration)
		    `(,(name field) 0 :type ,fbty))

		   ;; tables
		   ((subtypep (type-of ty) 'Table)
		    `(,(name field)0 :type fb-table))

		   ;; structs
		   ((subtypep (type-of ty) 'Struct)
		    `(,(name field) 0 :type ,lbty))

		   ;; arrays
		   ((subtypep (type-of ty) 'CArray)
		    `(,(name field) 0 :type ,lbty))

		   ;; that should cover everything
		   (t
		    (error "No representation for ~a" ty)))))))

    ;; create the fields
    (let ((fields (remove-if #'null (mapcar #'create-field (fields ty))))
	  (lbty (representation ty)))

      (log:debug "Creating ~a to represent fb type ~a" lbty (name ty))

      (eval `(defbinary ,lbty (:byte-order :little-endian)
	       ,@fields)))))


(defclass Enumeration (FB-Record)
  ())


(defclass Struct (FB-Record)
  ())


(defmethod initialize-instance :after ((ty Struct) &key &allow-other-keys)
  (declare (optimize debug))

  (flet ((create-field (field)
	   "Return the code for FIELD at the given OFFSET."
	   ;; ignore any deprecated fields
	   (unless (deprecated-p field)
	       (let ((lbty (representation field)))

		 ;; add the field
		 (cond
		   ;; numbers
		   ((subtypep lbty '(or unsigned-byte
				     signed-byte
				     float
				     double))
		    `(,(name field) 0 :type ,lbty))

		   (t
		    (error "Can't handle structure field for ~a" lbty)))))))

    (let ((fields (remove-if #'null (mapcar #'create-field (fields ty))))
	  (lbty (representation ty)))

      (eval `(defbinary ,lbty (:byte-order :little-endian)
	       ,@fields)))))


;;; ---------- Builder functions ----------

;;; The approach here is to read in the schema as s-expressions, expressed in
;;; terms of flatbuffers types, and convert it into type metaobjects expressed
;;; in terms of LISP-BINARY types. The latter can be the built-in types for
;;; LISP-BINARY or the names of structures created wuth DEFBINARY.

(defun make-lisp-binary-type-name (stem fbty)
  "Return a new symbol to name a binary representation of FBTY defined with DEFBINARY.

STEM will be used as the first part of the name. This is just to make
debugging slightly easier."
  (intern (upcase (format nil "~a-~a" stem fbty))))


(defun make-field (name vars)
  "Make a metaobject for a field NAME over the given VARS.

This may involve making some auxiliary LISP-BINARY types to represent
the contents of the field."
  (declare (optimize debug))

  (let* ((fbty (getassoc :type vars))
	 (meta (safe-cdr (assoc :metadata vars)))
	 (deprecated-p (and (not (null meta))
			    (assoc 'deprecated meta)))
	 (lbty (or (lisp-binary-type-for-fb-type fbty)

		   ;; not a base type, check for constructed types
		   (if-let ((ty (fb-complex-type-to-type fbty)))
		     ;; we have a constructed type, use its representation
		     (representation ty)

		     ;; we don't have a type constructed yet, build it
		     (if (fb-array-type-p fbty)
			 ;; array types
			 (let* ((fbet (fb-array-type-element-type fbty))
				(et (lisp-binary-type-for-fb-type fbet))
				(lbty (make-lisp-binary-type-name "fb-array" name)))

			   (make-instance 'CArray :name lbty
						  :fb-type fbty
						  :element-type et
						  :representation lbty)
			   lbty)

			 ;; anything else we can't deal with
			 ;TODO: Handle forward references to complex types
			 (error "No type for ~a" fbty))))))

    (make-instance 'Field :name name
			  :fb-type fbty
			  :type (if (fb-complex-type-p fbty)
				    (fb-complex-type-to-type fbty))
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
			    elements (list '() 0))))))

    (make-instance 'Enumeration :name name
				:fb-type fbety
				:fields fields))


(defun make-table (name elements)
  "Make a description for table NAME with fields built from ELEMENTS.

This may result in code to build additional types needed by the fields."
  (declare (optimize debug))
  (let ((lbty (make-lisp-binary-type-name "fb-table" name))
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

  (let ((lbty (make-lisp-binary-type-name "fb-struct" name))
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

    ;; create the necessary classes
    (mapc #'make-object schema)

    ;; return the description of the root type
    (fb-complex-type-to-type root-type)))


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
