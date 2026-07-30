;;;; Tensor interface
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


;;; ---------- The abstract type ----------

;;; A lot of operation names take the form TENSOR-, which would
;;; usually be considered bad practice (including the name of the
;;; class into the operation) but is used to match the ARRAY-
;;; operations in Common Lisp.

(defclass Tensor ()
  ((rank
    :reader tensor-rank
    :documentation "The rank (order, number of dimensions) of the tensor.")
   (dimensions
    :initform nil
    :reader tensor-dimensions
    :documentation "The dimensions of the tensor as a list of non-negative integers.")
   (element-type
    :initform 'number
    :initarg :element-type
    :reader tensor-element-type
    :documentation "The type of elements."))
  (:documentation "The base class of tensors.

Tensors are multidimensional arrays of things, usually numbers."))


(defmethod initialize-instance :after ((t1 Tensor) &key dimensions &allow-other-keys)
  (unless dimensions
    (error "Need a list of dimensions."))

  (setf (slot-value t1 'rank) (length dimensions)))


(defun tensor-dimension (t1 axis)
  "Return the dimension of T1 along AXIS.

Axes are numbered from 0 to (1- TENSOR-RANK)"
  (unless (and (>= axis 0)
	       (< axis (tensor-rank t1)))
    (error 'incomptible-axis t1:t1 t1 :axis axis))

  (elt (tensor-dimensions t1) axis))


(defun tensor-total-size (t1)
  "Return the number of elements in T1."
  (foldr #'* (tensor-dimensions t1) 1))


(defun tensor-p (t1)
  "Test whether T1 is a tensor."
  (subtypep (type-of t12) 'Tensor))


(defun ensure-index-compatible (t1 indices)
  "Ensure INDICES are compatible with T1.

There must be as many indices as the rank of T1, and each index
must lie in the range of the corresponding axis."
  (unless (and (= (length indices) (tensor-rank t1))
	       (every (lambda (j)
			(let ((i (elt indices j)))
			  (and (> i 0)
			       (<= i (tensor-dimension t1 j)))))
		      (iota (length indices))))
    (error 'incompatible-index :t1 t1 :index indices)))


(defun ensure-pointwise-compatible (t1 t2 t3)
  "Ensure T1, T2, and T3 are compatible for pointwise operations.

At most one of T1 and T2 may be a scalar. If both are tensors, they
must be compatible.

If T3 is non-NIL, it must be cmpatible with the tensors T1 and/or T2."
  (let ((tcheck (cond ((and (tensor-p t1)
			    (tensor-p t2))
		       (unless (equal (tensor-dimensions t1) (tensor-dimensions t2))
			 (error 'incompatible-dimensions-pointwise :t1 t1 :t2 t2))
		       t1)

		      ((tensor-p t1)
		       t1)

		      ((tensor-p t2)
		       t2)

		      (t
		       (error 'tensor-needed)))))

    (unless (null t3)
      (unless (equal (tensor-dimensions tcheck) (tensor-dimensions t3))
	(error 'incompatible-dimensions-pointwise :t1 tcheck :t2 t3)))))


;;; ---------- Access ----------

;;; We check indices early to make sure they're legal.

(defmethod aref :before ((t1 Tensor) &rest indices)
  (ensure-index-compatible t1 indices))


(defmethod (setf aref) :before (v (t1 Tensor) &rest indices)
  (ensure-index-compatible t1 indices))


;;; These two macros are used for writing AREF and (SETF AREF) methods.
;;; Because the number indices is unknown at compile time we change the
;;; signature to accept the index as an array rather than inline.

(defmacro getf-aref (t1 indices)
  "Return the value of the element at INDICES in T1.

This is equivalent to (AREF T1 . INDICES).

This is a workaround to make it easier to use AREF when the number
of indices (and the rank of T1) are unknown."
  (apply #'aref (cons t1 indices)))


(defmacro setf-aref (t1 indices v)
  "Set the value of element at INDICES of T1 to V.

This is equivalent to (SETF (AREF T1 . INDICES) V).

This is a workaround to make it easier to use AREF when the number
of indices (and the rank of T1) are unknown."
  `(apply #'(setf aref) (cons ,v (list ,t1 ,@indices))))


;;; ---------- Function application ----------

(defgeneric .apply-with-index (f t1 &key destination all)
  (:documentation "Call F for each index of T1.

F should take two arguments, an index of T1 and the value at that index.
Its result is used to construct the result tensor.

DESTINATION, if present, should be a tensor compatible with T1 (it may
refer to T1 safely). It omitted, a new tensor will be created.

If ALL is non-NIL then F is applied to all indices of T1. If ALL is NIL
(the default) then a method may choose to (and should) only apply F to
non-zero elements.

Return the results tensor."))


(defun .apply (f t1 &key destination all)
  "Apply function F pointwise to T1.

F should be a function of one variable compatible with the element
type of T1.

DESTINATION, if present, should be a tensor compatible with T1 (it may
refer to T1 safely). It omitted, a new tensor will be created.

If ALL is non-NIL then F is applied to all indices of T1. If ALL is NIL
(the default) then a method may choose to (and should) only apply F to
non-zero elements.

RReturn the results tensor."
  (.apply-with-index (lambda (i v)
		       (declare (ignore i))
		       (funcall f v))
		     t1 :destination destination :all all))


;;; ---------- Maths operations ----------

(defgeneric .+ (t1 t2 &key destination)
  (:documentation "Add elements of T1 and T2 pointwise.

One or neither of T1 and T2 may be a scalar. If both are tensors, they
must have compatible rank and dimensions.

DESTINATION, if present, should be a tensor compatible with T1 and
T2 (it may refer to either of them safely). It omitted, a new tensor
will be created.

Return the results tensor.")
  (:method ((t1 Tensor) (t2 Tensor) &key destination)
    (.apply #'+ t1 t2 :destination destination :all t))

  (:method ((t1 Tensor) (t2 Number) &key destination)
    (.apply (rcurry #'+ t2) :destination destination :all t))

  (:method ((t1 Number) (t2 Tensor) &key destination)
    (.apply (curry #'+ t1) :destination destination :all t)))


(defgeneric .- (t1 t2 &key destination)
  (:documentation "Subtract elements of T1 and T2 pointwise.

One or neither of T1 and T2 may be a scalar. If both are tensors, they
must have compatible rank and dimensions.

DESTINATION, if present, should be a tensor compatible with T1 and
T2 (it may refer to either of them safely). It omitted, a new tensor
will be created.

Return the results tensor.")
  (:method ((t1 Tensor) (t2 Tensor) &key destination)
    (.apply #'- t1 t2 :destination destination :all t))

  (:method ((t1 Tensor) (t2 Number) &key destination)
    (.apply (rcurry #'- t2) :destination destination :all t))

  (:method ((t1 Number) (t2 Tensor) &key destination)
    (.apply (curry #'- t1) :destination destination :all t)))


(defgeneric .* (t1 t2 &key destination)
  (:documentation "Multiply elements of T1 and T2 pointwise.

One or neither of T1 and T2 may be a scalar. If both are tensors, they
must have compatible rank and dimensions.

DESTINATION, if present, should be a tensor compatible with T1 and
T2 (it may refer to either of them safely). It omitted, a new tensor
will be created.

Return the results tensor.")
  (:method ((t1 Tensor) (t2 Tensor) &key destination)
    (.apply #'* t1 t2 :destination destination :all t))

  (:method ((t1 Tensor) (t2 Number) &key destination)
    (.apply (rcurry #'* t2) :destination destination :all t))

  (:method ((t1 Number) (t2 Tensor) &key destination)
    (.apply (curry #'* t1) :destination destination :all t)))


(defgeneric ./ (t1 t2 &key destination)
  (:documentation "Divide elements of T1 and T2 pointwise.

One or neither of T1 and T2 may be a scalar. If both are tensors, they
must have compatible rank and dimensions.

DESTINATION, if present, should be a tensor compatible with T1 and
T2 (it may refer to either of them safely). It omitted, a new tensor
will be created.

Return the results tensor.")
  (:method ((t1 Tensor) (t2 Tensor) &key destination)
    (.apply #'/ t1 t2 :destination destination :all t))

  (:method ((t1 Tensor) (t2 Number) &key destination)
    (.apply (rcurry #'/ t2) :destination destination :all t))

  (:method ((t1 Number) (t2 Tensor) &key destination)
    (.apply (curry #'/ t1) :destination destination :all t)))


(defgeneric .max (t1 t2 &key destination)
  (:documentation "Return the pointwise maximum of elements of T1 and T2.")
  (:method ((t1 Tensor) (t2 Tensor) &key destination)
    (.apply #'max t1 t2 :destination destination :all t))

  (:method ((t1 Tensor) (t2 Number) &key destination)
    (.apply (rcurry #'max t2) :destination destination :all t))

  (:method ((t1 Number) (t2 Tensor) &key destination)
    (.apply (curry #'max t1) :destination destination :all t)))


(defgeneric .min (t1 t2 &key destination)
  (:documentation "Return the pointwise minimum of elements of T1 and T2.")
  (:method ((t1 Tensor) (t2 Tensor) &key destination)
    (.apply #'min t1 t2 :destination destination :all t))

  (:method ((t1 Tensor) (t2 Number) &key destination)
    (.apply (rcurry #'min t2) :destination destination :all t))

  (:method ((t1 Number) (t2 Tensor) &key destination)
    (.apply (curry #'min t1) :destination destination :all t)))


(defgeneric @ (t1 t2 &key destination)
    (:documentation "Multiply T1 and T2.

T1 and T2 must have compatible rank and dimensions for multiplication.

DESTINATION, if present, should be a tensor compatible with T1 (it may
refer to T1 safely). It omitted, a new tensor will be created.

Return the results tensor."))
