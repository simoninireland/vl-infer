;;;; Top-level test package
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


(defpackage vl-infer/test
  (:use :cl :alexandria :fiveam :vl-infer/flatbuffers)
  (:local-nicknames
   (:fb :vl-infer/flatbuffers)))

(in-package :vl-infer/test)

(def-suite vl-infer/utils)        ; utils
(def-suite vl-infer/flatbuffers)  ; flatbuffers handling
(def-suite vl-infer/tensors)      ; matrices and tensors
(def-suite vl-infer/ml)           ; inference

(defparameter *this-directory* (pathname-directory #.(or *compile-file-truename* *load-truename*))
	      "The pathname of this directory.")

(defun load-test-file (fn)
  "Load file named FN relative to *THIS-DIRECTORY*."
  (make-pathname :directory *this-directory* :name fn))
