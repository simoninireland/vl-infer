;;;; Tensor conditions
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

(in-package :vl-infer/tensors)


(define-condition incompatible-dimensions-pointwise (error)
  ((t1
    :initarg :t1
    :documentation "The first tensor.")
   (t2
    :initarg :t2
    :documentation "The second tensor."))
  (:report (lambda (c str)
	     (format str "Tensors have incompatible dimensions for pointwise operations (~a and ~a)"
		     (tensor-dimensions (slot-value c 't1))
		     (tensor-dimensions (slot-value c 't2)))))
  (:documentation "Error signalled when two tensors are incompatible for pointwise operations.

Tensors in pointwise operations must have the same rank and dimensions."))


(define-condition incompatible-dimensions-for-multiplication (error)
  ((t1
    :initarg :t1
    :documentation "The first tensor.")
   (t2
    :initarg :t2
    :documentation "The second tensor."))
  (:report (lambda (c str)
	     (format str "Tensors have incompatible dimensions for multiplication (~a and ~a)"
		     (tensor-dimensions (slot-value c 't1))
		     (tensor-dimensions (slot-value c 't2)))))
  (:documentation "Error signalled when two tensors are incompatible for multiplication."))


(define-condition tensor-needed (error)
  ()
  (:report (lambda (c str)
	     (format str "A tensor is needed")))
  (:documentation "Error signalled when a tensor was not supplied when needed.

This can happen when a tensor operator is mistakenly applied to
two scalars."))


(define-condition incompatible-index (error)
  ((t1
    :initarg :t1
    :documentation "The tensor.")
   (index
    :initarg :index
    :documentation "The index."))
  (:report (lambda (c str)
	     (format str "Index ~a is incompatible with tensor of dimensions ~a"
		     (slot-value c 'index) (tensor-dimensions (slot-value c 't1)))))
  (:documentation "Error signalled when an incompatible index is applied.

A valid index has the same number of dimensions as the tensor, with each element
lying in the range of the corresponding axis."))


(define-condition incompatible-axis (error)
  ((t1
    :initarg :t1
    :documentation "The tensor.")
   (axis
    :initarg :axis
    :documentation "The axis."))
  (:report (lambda (c str)
	     (format str "Axis ~a is incompatible with tensor of dimensions ~a"
		     (slot-value c 'axis) (tensor-dimensions (slot-value c 't1)))))
  (:documentation "Error signalled when an incompatible axis is applied.

A valid axis runs from 0 to (1- (TENSOR-DIMENSIONS t1))."))
