;;;; Dense tensor
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


;;; ---------- The type ----------

(defclass Dense (Tensor)
  ((elements
    :reader tensor-elements
    :documentation "The elements of the tensor."))
  (:documentation "A dense tensor.

Dense tensors store their elements directly."))


(defmethod initialize-instance :after ((t1 Dense) &key dimensions element-type)
  (setf (slot-value t1 'elements) (make-array dimensions :element-type element-type)))


(defun make-dense-tensor (dimensions &key element-type)
  "Return a new dense tensor with the given DIMENSIONS."
  (make-instance 'Dense :dimensions dimensions :element-type element-type))


;;; ---------- Access ----------

(defmethod aref ((t1 Dense) &rest indices)
  (getf-aref t1 indices))


(defmethod (setf aref) (v (t1 Dense) &rest indices)
  (setf-aref (slot-value t1 'elements) indices v))


;;; ---------- Function application----------

(defmethod .apply-with-index (f (t1 Dense) &key destination all)
  (let ((i (make-list (tensor-rank t1) :initial-element 0))
	(t3 (if destination
		(ensre-pointwise-copmpatible t1 destination)
		(make-dense-tensor (tensor-dimensions t1) :element-type (tensor-element-type t1)))))

    (dotimes (n (tensor-total-size t1))
      ;; do the call
      (let ((v (get-aref t1 i)))
	(when (/= v 0)
	  (setf-aref t3 i (funcall f i v))))

      ;; increment the index
      (incf (car i))
      (dotimes (j (length i))
	(when (> (elt i j) (tensor-dimension t1 j))
	  (setf (elt i j) 0)
	  (incf (elt i (1+ j))))))

    ;; return the results tensor
    t3))
