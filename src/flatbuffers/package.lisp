;;;; Package definition for flatbuffers parser
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

(in-package :common-lisp-user)


(defpackage vl-infer/flatbuffers
  (:use :cl :alexandria :esrap :parser.common-rules :parse-float :lisp-binary)
  (:import-from :str
		#:concat
		#:substring
		#:starts-with-p
		#:upcase
		#:string-case)
  (:import-from :parser.common-rules
		#:defrule/s
		#:whitespace
		#:whitespace?
		#:whitespace*
		#:whitespace+)

  (:export
   ;; base types
   #:bool
   #:byte #:ubyte #:int8 #:uint8
   #:short #:ushort #:int16 #:uint16
   #:int #:uint #:int32 #:uint32
   #:long #:ulong #:int64 #:uint64
   #:float #:float32
   #:double #:float64

   ;; structure base classes
   #:Table
   #:Structure
   #:Enumeration

   ;; schema parser
   #:parse-fbs-schema

   ;; flatbuffers reader
   #:make-schema
   #:read-fbs
   ))
