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

(defmacro deftoken (name rule &body options)
  "Declare a optionally whitespace-delimited token NAME.

Add explicit whitespace parsesrs where needed."
  `(defrule ,name
       (and whitespace* ,rule whitespace*)
     ,@options))


(defmacro deftoken* (name rule)
  "Define a simple token NAME that always returns NIL.

RULE will typically be a single string or character."
  `(deftoken ,name
       ,rule
     (:constant nil)))


(defmacro defsymbol (name rule)
  "Define a simple token NAME that returns NAME as a symbol.

RULE will usually be a single string or character."
  `(deftoken ,name
       (and whitespace* ,rule whitespace*)
     (:constant ',name)))


(defmacro deflistrule (name rule &body options)
  "Define a rule NAME that matches a comma-separated list of RULE."
  `(defrule ,name
       (and ,rule (? (and comma ,rule)))
     ,@options))


;;; ---------- Characters ----------

(defrule point #\.)
(defrule sign (or #\+ #\-))
(defrule exp (or #\E #\e))
(defrule power (or #\P #\p))
(defrule hex (and "0" (or #\X #\x)))

(defrule digit (character-ranges (#\0 #\9)))
(defrule xdigit (character-ranges (#\0 #\9) (#\A #\F) (#\a #\f)))


;;; ---------- Tokens ----------

;; delimiters
(deftoken* semi ";")
(deftoken* comma ",")
(deftoken* colon ":")
(deftoken* dot ".")
(deftoken* quotes "\"")
(deftoken* equals-sign "=")
(deftoken* open-curly "{")
(deftoken* close-curly "}")
(deftoken* open-square "[")
(deftoken* close-square "]")
(deftoken* open-round "(")
(deftoken* close-round ")")

;; keywords
(defsymbol include "include")
(defsymbol attribute "attribute")
(defsymbol namespace "namespace")
(defsymbol table "table")
(defsymbol struct "struct")
(defsymbol enum "enum")
(defsymbol union "union")
(defsymbol rpc-service "rpc_service")
(defsymbol root-type "root_type")
(defsymbol file-extension "file_extension")
(defsymbol file-identifier "file_identifier")


;; strings
(defrule string-constant (and whitespace* quotes (* (not quotes)) quotes whitespace*)
  (:destructure (w1 q1 s q2 w2)
		(declare (ignore w1 q1 q2 w2))
		(text s)))


;; integers
(defrule dec-integer-constant
    (and (? sign) (+ digit))
  (:lambda (l)
    (parse-integer (text l) :radix 10)))

(defrule hex-integer-constant
    (and (? sign) hex (+ xdigit))
  (:lambda (l)
    (parse-integer (text l) :radix 16)))

(deftoken integer-constant
    (or dec-integer-constant hex-integer-constant))


;; floats
(defrule dec-float-constant
    (and (? sign)
	 (or (and point (+ digit))
	     (and (+ digit) (? (and point (+ digit)))))
	 (? (and exp (? sign) (+ digit))))
  (:lambda (l)
    (parse-float (text l) :radix 10 :exponent-character #\E)))

(defrule hex-float-constant
    (and (? sign)
	 hex
	 (or (and point (+ xdigit))
	     (and (+ xdigit) (? (and point (+ xdigit)))))
	 (? (and power (? sign) (+ xdigit))))
  (:lambda (l)
    (parse-float (text l) :radix 16 :exponent-character #\P)))

(defrule special-float-constant
    (and (? sign) (or "nan" "inf" "infinity"))
  (:text t))

(deftoken float-constant
    (or dec-float-constant
	hex-float-constant
	special-float-constant))


;; booleans
(deftoken boolean-constant
    (or "true" "false"))


;; identifiers
(defrule identifier			; raw form
    (and (character-ranges (#\a #\z) (#\A #\Z) #\_)
	 (* (character-ranges (#\a #\z) (#\A #\Z) (#\0 #\9) #\_)))
  (:lambda (l)
    (intern (upcase (text l)))))

(deftoken ident			     ; optionally whitespace-delimited
    identifier)


;; types
(defrule simple-type (or "bool"
			 "byte" "ubyte" "int8" "uint8"
			 "short" "ushort" "int16" "uint16"
			 "int" "uint" "int32" "uint32"
			 "long" "ulong" "int64" "unit64"
			 "float" "float32"
			 "double" "float64"
			 "string")
  (:lambda (l)
    (intern (upcase (text l)))))
(deftoken complex-type (or (and open-square type close-square)
			   ident))

(defrule type (or simple-type complex-type))


;;; ---------- Rules ----------

;; comments
(defrule comment (and "//" (* (not #\Newline)))
  (:constant nil))


;; overall schema
(defrule schema (and whitespace*
		     (? comment)
		     whitespace*
		     (? include)
		     whitespace*
		     (* (and (or comment
				 namespace-decl type-decl enum-decl root-decl
				 file-extension-decl file-identifier-decl)
			     whitespace*))))

;; inclusions
(defrule include (and include string-constant semi))


;; namespace and attributes
(defrule namespace-decl (and namespace identifier (* (and dot identifier)) semi)
  (:destructure (nst ns1 nss w1)
		(declare (ignore w1))
		(let ((ns (if (null nss)
			      ns1
			      (cons ns1 (mapcar #'cadr nss)))))
		  `(,nst ,ns))))
(defrule attribute-decl (and attribute (or ident (and quotes ident quotes)) semi))


;; types
(defrule root-decl (and root-type ident semi))

(defrule type-decl (and (or table struct) ident metadata open-curly (* field-decl) close-curly))
(defrule enum-decl (and (or (and enum ident colon type)
			    (and union ident))
			metadata
			open-curly (? (and enumval-decl (* (and comma enumval-decl)))) close-curly)
  (:destructure (h &rest fields)
		;; remove the unnecessary nesting caused by the OR rule
		(append h fields)))


(defrule field-decl (and ident colon type (? (and equals-sign (or scalar ident))) metadata semi)
  (:destructure (id w1 ty iv meta w4)
		(declare (ignore w1 w4))
		`(,id (:type ,ty) ,@(if iv `((:default ,iv))) ,meta)))
(defrule enumval-decl (and ident (? (and equals-sign integer-constant)) metadata)
  (:destructure (id expl meta)
		`(,id ,@(if expl `((:default ,expl))) ,meta)))


;; metadata
(deflistrule ident-single-value?-list
    ident-single-value?)
(defrule metadata (and whitespace*
		       (? (and open-round ident-single-value?-list close-round
			       whitespace*)))
  (:destructure (w1 m)
		(declare (ignore w1))
		(unless (null m)
		  ;; we have metadata
		  (cons :metadata m))))


;; RPC declarations
(defrule rpc-decl (and rpc-service whitespace+ ident open-round (+ rpc-method) close-round))
(defrule rpc-method (and ident whitespace? open-round ident close-round whitespace* colon ident metadata semi))

;; values and assignments
(defrule scalar (or boolean-constant integer-constant float-constant))
(defrule ident-value (and ident colon value))
(defrule ident-single-value (and ident colon single-value))
(defrule ident-single-value? (and ident (? (and colon single-value))))
(defrule object (and open-round (? (and ident-value (? (and comma ident-value)))) close-round))
(defrule single-value (or scalar string-constant))
(deflistrule value-list
    value)
(defrule value (or single-value object (and open-squarevalue-list close-square)))

;; file information
(defrule file-extension-decl (and file-extension string-constant semi))
(defrule file-identifier-decl (and file-identifier string-constant semi))


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

    ;; parse the schema
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
