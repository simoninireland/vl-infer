;;;; Package definition for tensor classes
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


(defpackage vl-infer/tensors
  (:use :cl :alexandria)

  (:export
   ;; classes
   #:Tensor
   #:Dense
   #:CSR

   ;; properties
   #:tensor-rank
   #:tensor-dimension
   #:tensor-dimensions
   #:tensor-elements
   #:tensor-total-size

   ;; constructors
   #:make-dense-tensor
   #:make-csr-tensor
   #:adjust-tensor

   ;; function application
   #:.apply
   #:.apply-with-index

   ;; maths operations
   #:@
   #:.+
   #:.-
   #:.*
   #:./
   #:.max
   #:.min

   ;; conditions
   #:incompatible-dimensions-pointwise
   #:incompatible-dimensions-for-multiplication
   #:incompatible-index
   #:incompatible-axis
   #:tensor-needed
   ))
