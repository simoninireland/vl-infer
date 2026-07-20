;;;; Flatbuffer schema parsing
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

;;; We use ESRAP to construct a parser for .fbs schema files, working directly
;;; from the official grammar (https://flatbuffers.dev/grammar/).


;;; ---------- Helper macros ----------

(defrule skippable
    parser.common-rules:whitespace+)

(defrule skippable?
    parser.common-rules:whitespace*)

(defmacro deftoken (name rule)
  "Define a token NAME using RULE returning NAME as a token."
  `(defrule/s ,name
       ,rule
     (:constant ',name)))


;;; ---------- Characters ----------

(defrule POINT #\.)
(defrule SIGN (or #\+ #\-))
(defrule EXP (or #\E #\e))
(defrule POWER (or #\P #\p))
(defrule HEX (and "0" (or #\X #\x)))

(defrule digit (character-ranges (#\0 #\9)))
(defrule xdigit (character-ranges (#\0 #\9) (#\A #\F) (#\a #\f)))


;;; ---------- Tokens ----------

;; delimiters
(deftoken SEMI ";")
(deftoken COMMA ",")
(deftoken COLON ":")
(deftoken DOT ".")
(deftoken QUOTES "\"")
(deftoken EQUALS-SIGN "=")
(deftoken OPEN-CURLY "{")
(deftoken CLOSE-CURLY "}")
(deftoken OPEN-SQUARE "[")
(deftoken CLOSE-SQUARE "]")
(deftoken OPEN-ROUND "(")
(deftoken CLOSE-ROUND ")")

;; keywords
(deftoken INCLUDE "include")
(deftoken ATTRIBUTE "attribute")
(deftoken NAMESPACE "namespace")
(deftoken TABLE "table")
(deftoken STRUCT "struct")
(deftoken ENUM "enum")
(deftoken UNION "union")
(deftoken RPC-SERVICE "rpc_service")
(deftoken ROOT-TYPE "root_type")
(deftoken FILE-EXTENSION "file_extension")
(deftoken FILE-IDENTIFIER "file_identifier")

;; special constants
(deftoken NAN "nan")
(deftoken INF "inf")
(deftoken INFINITY "infinity")
(deftoken TRUE "true")
(deftoken FALSE "false")


;; strings
(defrule string-constant (and QUOTES (* (not QUOTES)) QUOTES)
  (:destructure (q1 s q2)
		(declare (ignore q1 q2))
		(text s)))


;; integers
(defrule dec-integer-constant
    (and (? SIGN) (+ digit))
  (:lambda (l)
    (parse-integer (text l) :radix 10)))

(defrule hex-integer-constant
    (and (? SIGN) HEX (+ xdigit))
  (:lambda (l)
    (parse-integer (text l) :radix 16)))

(defrule integer-constant
    (or dec-integer-constant hex-integer-constant))


;; floats
(defrule dec-float-constant
    (and (? SIGN)
	 (or (and POINT (+ digit))
	     (and (+ digit) (? (and POINT (+ digit)))))
	 (? (and EXP (? SIGN) (+ digit))))
  (:lambda (l)
    (parse-float (text l) :radix 10 :exponent-character #\E)))

(defrule hex-float-constant
    (and (? SIGN)
	 hex
	 (or (and POINT (+ xdigit))
	     (and (+ xdigit) (? (and POINT (+ xdigit)))))
	 (? (and POWER (? SIGN) (+ xdigit))))
  (:lambda (l)
    (parse-float (text l) :radix 16 :exponent-character #\P)))

(defrule special-float-constant
    (and (? SIGN) (or NAN INF INFINITY)))

(defrule float-constant
    (or dec-float-constant
	hex-float-constant
	special-float-constant))


;; booleans
(defrule boolean-constant
    (or TRUE FALSE))


;; identifiers
(defrule/s ident
    (and (character-ranges (#\a #\z) (#\A #\Z) #\_)
	 (* (character-ranges (#\a #\z) (#\A #\Z) (#\0 #\9) #\_)))
  (:lambda (l)
    (intern (upcase (text l)))))


;; types
(defrule/s simple-type (or "bool"
			   "byte" "ubyte" "int8" "uint8"
			   "short" "ushort" "int16" "uint16"
			   "int" "uint" "int32" "uint32"
			   "long" "ulong" "int64" "unit64"
			   "float" "float32"
			   "double" "float64"
			   "string")
  (:lambda (l)
    (intern (upcase (text l)))))
(defrule array-type (and open-square/?s (or simple-type/?s ident/?s) close-square/?s))
(defrule complex-type (or array-type
			  ident))

(defrule type (or simple-type/?s complex-type))


;;; ---------- Rules ----------

;; comments
(defrule comment (and "//" (* (not #\Newline)) #\Newline)
  (:constant nil))


;; overall schema
(defrule header (and (* (or whitespace+ comment include))))
(defrule schema (and header
		     (* (and whitespace*
			     (or comment
				 namespace-decl type-decl enum-decl union-decl
				 root-decl
				 file-extension-decl file-identifier-decl)))))

;; inclusions
(defrule include (and include/s string-constant semi))


;; namespace and attributes
(defrule namespace-decl (and NAMESPACE/s identifier (* (and dot identifier)) semi/?s)
  (:destructure (nst ns1 nss w1)
		(declare (ignore w1))
		(let ((ns (if (null nss)
			      ns1
			      (cons ns1 (mapcar #'cadr nss)))))
		  `(,nst ,ns))))
(defrule attribute-decl (and attribute/s (or ident (and quotes ident quotes)) semi/?s))


;; root type
(defrule root-decl (and root-type/s IDENT/?s semi/?s)
  (:lambda (l)
    (list (elt l 0) (elt l 1))))


;; tables
(defrule type-decl (and (or TABLE/s STRUCT/s) IDENT/?s
			(? metadata)
			open-curly/?s (* (or field-decl comment)) close-curly/?s)
  (:lambda (l)
    (list (elt l 0) (elt l 1) (elt l 2) (elt l 4))))

(defrule field-decl (and ident/?s colon/?s type whitespace* (? (and equals-sign/?s (or scalar ident) whitespace*))
			 (? metadata) semi/?s)
  (:destructure (id colon ty w1 iv meta w4)
		`(,id (:type ,ty)
		      ,@(if iv `((:default ,(cadr iv))))
		      ,@(if meta `((:metadata ,meta))))))


;; enumerations
(defrule enum-decl (and ENUM/s ident/?s colon/?s type whitespace*
			(? metadata)
			open-curly/?s (? (and enumval-decl (* (and comma/?s enumval-decl)))) close-curly/?s)
  (:destructure (tag id colon ty w1 meta ocurl fs ccurl)
		(list tag id ty meta fs)))

(defrule enumval-decl (and ident/?s (? (and equals-sign/?s integer-constant)) whitespace*
			   (? metadata)
			   whitespace?)
  (:destructure (id iv w1 meta w2)
		`(,id
		  ,@(if iv
			`((:default ,(cadr iv))))
		  ,@(if meta
			`((:metadata ,(car iv)))))))


;; unions
(defrule union-decl (and UNION/s ident/?s whitespace*
			 (? metadata)
			 open-curly/?s (? (and enumval-decl (* (and comma/?s enumval-decl)))) close-curly/?s)
  (:lambda (l)
    (list (elt l 0) (elt l 1) (elt l 2) (elt l 3))))


;; metadata
(defrule metadata (and open-round/?s ident-single-value? close-round/?s)
  (:lambda (l)
    (declare (optimize debug))

    (let ((iv (elt l 1)))
      (if (null (cadr iv))
	  (list (car iv) t)
	  iv))))


;; RPC declarations
(defrule rpc-decl (and rpc-service/s ident open-round (+ rpc-method) close-round))
(defrule rpc-method (and ident/?s open-round/?s ident/?s close-round/?s colon/?s ident/?s metadata semi))

;; values and assignments
(defrule scalar (or boolean-constant integer-constant float-constant))
(defrule ident-value (and ident/?s colon/?s value))
(defrule ident-single-value (and ident/?s colon/?s single-value))
(defrule ident-single-value? (and ident/?s (? (and colon/?s single-value))))
(defrule object (and open-round/?s (? (and ident-value (? (and comma/?s ident-value)))) close-round/?s))
(defrule single-value (or scalar string-constant))
(defrule value (or single-value object (and open-square/?s (and value (* (and comma/?s value))) close-square/?s)))

;; file information
(defrule file-extension-decl (and file-extension/s string-constant semi))
(defrule file-identifier-decl (and file-identifier/s string-constant semi))


;;; ---------- Top-level parser function ----------

(defun parse-fbs-schema (fbs)
  "Parse a .fbs file containing a flatbuffers schema.

FBS can be a pathname, a stream, or a string."
  (let (buf)
    (cond ((pathnamep fbs)
	   ;; pathname, real from a file
	   (with-open-file (str fbs :direction :input)
	     (setq buf (make-string (file-length str)))
	     (read-sequence buf str)))

	  ((streamp fbs)
	   ;; stream, read from it
	   (setq buf (make-string (file-length fbs)))
	   (read-sequence buf fbs))

	  ((stringp fbs)
	   ;; string, use literally
	   (setq buf fbs))

	  (t
	   (error "Can't parse schema from object ~a" fbs)))

    ;; reset the types and vtables tables
    ;; These need to be SETQ and not LET because we fill them in
    ;; during parsing
    (setq *object-types* (make-hash-table))
    (setq *object-type-vtable* (make-hash-table))

    ;; parse the schema, deleting the unnecessary structure
    (car (denil (parse 'schema buf)))))


(defun fbs-root-type (schema)
  "Extract the root object type of SCHEMA.

This will be either a type name declared by the root_type element of
the schema grammar, or the first table declared."
  (if-let ((rt (assoc 'root-type schema)))
    ;; explicit root_type declared
    (cadr rt)

    ;; extract the first table
    (if-let ((t1 (assoc 'table schema :key #'car)))
      (safe-cadr t1)

      ;; no tables
      (error "No tables defined in schema"))))
