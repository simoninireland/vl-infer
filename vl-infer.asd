;;;; System definitions
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


(asdf:defsystem "vl-infer"
  :description "A machine learning inference accelerator builder."
  :author "Simon Dobson <simoninireland@gmail.com"
  :version (:read-file-form "version.sexp")
  :license "GPL3"
  :depends-on ("alexandria" "str" "esrap" "parser.common-rules" "parse-float" "lisp-binary")
  :pathname "src/"
  :serial t
  :components (;; flatbuffers
	       (:module "flatbuffers"
		:components ((:file "package")
			     (:file "utils")
			     (:file "builder")
			     (:file "parser"))))
  :in-order-to ((test-op (test-op "vl-infer/test"))))


(asdf:defsystem "vl-infer/test"
  :description "Global tests of vl-infer."
  :depends-on ("alexandria" "vl-infer" "fiveam")
  :pathname "test/"
  :serial t
  :components ((:file "package")
	       (:file "test-utils")
	       (:file "test-flatbuffers-parser"))
  :perform (test-op (o c) (uiop:symbol-call :fiveam '#:run-all-tests)))
