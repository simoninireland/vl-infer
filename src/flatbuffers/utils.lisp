;;;; Flatbuffer helper utilities
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


;;; ---------- Numbers to and from arrays of bytes ----------

(defmacro little-endian (ptr bytes)
  "Return the value of PTR in BYTES bytes."
  (let ((pos (mapcar (curry #'* 8) (iota bytes))))
     `(mapcar (lambda (i)
		(ldb (byte 8 i) ,ptr))
	      ',pos)))


(defun 16-bits-little-endian (ptr)
  "Return PTR as two bytes, little-endian."
  (little-endian ptr 2))


(defun 32-bits-little-endian (ptr)
  "Return PTR as four bytes, little-endian."
 (little-endian ptr 4))


(defun 64-bits-little-endian (ptr)
  "Return PTR as eight bytes, little-endian."
  (little-endian ptr 8))


(defun from-little-endian (bs)
  "Convert BS bytes to integer as little-endian."
  (let ((v 0))
    (dotimes (i (array-dimension bs 0))
      (setf v (+ v (ash (aref bs i) (* i 8)))))
    v))


;;; ---------- Byte sizes for different types ----------

(defun bytes-for (type)
  "Return the number of bytes needed to represent an instance of TYPE.

Return NIL if there is no representation."
  (cond
    ;; unsigned fixed-width
    ((subtypep type '(unsigned-byte 16))
     2)
    ((subtypep type '(unsigned-byte 32))
     4)
    ((subtypep type '(unsigned-byte 64))
     8)

    ;; signed fixed-width
    ((subtypep type '(signed-byte 16))
     2)
    ((subtypep type '(signed-byte 32))
     4)
    ((subtypep type '(signed-byte 64))
     8)

    ;; strings are represented as long offsets into the strings table
    ((subtypep type 'string)
     4)

    ;; classes are represented as long offsets into the vtables table
    ((subtypep type 'standard-object)
     4)))


;;; ---------- List utilities ----------

;; Folds

;; These are just wrappers around REDUCE, but I find them easier to remember.

(defun foldl (fun l init)
  "Fold the values of L left through FUN, starting with initial value INIT."
  (reduce fun l :from-end t :initial-value init))


(defun foldr (fun l init)
  "Fold the values of L rightwards through FUN, starting with INIT."
  (reduce fun l :initial-value init))


(defun foldr-over-null (fun l init)
  "Fold FUN right over L starting with INIT, ignoring nulls.

This is like FOLDR except that a null in either the accumulated total
or one of the values automatically returns the other value."
  (flet ((fun-null (a v)
	   (cond ((null a)
		  v)
		 ((null v)
		  a)
		 (t
		  (funcall fun a v)))))

    (foldr #'fun-null l init)))


;; Zips

(defun zip (l1 l2)
  "Zip corresponding elements of L1 and L2.

The lists must have equal lengths."
  (cond ((null l1)
	 (if (null l2)
	     '()
	     (error "Lists have unequal lengths whe zipping (~a and ~a)" l1 l2)))
q
	((null l2)
	 (error "Lists have unequal lengths whe zipping (~a and ~a)" l1 l2))

	(t
	 (cons (list (car l1) (car l2))
	       (zip (cdr l1) (cdr l2))))))

(defun zip-without-null (xs ys)
  "Zip lists XS and YS when elements are not null.

If either element is null, the pair is omitted."
  (when (not (or (null xs)
		 (null ys)))
    (if (or (null (car xs))
	    (null (car ys)))
	(zip-without-null (cdr xs) (cdr ys))

	(cons (list (car xs) (car ys))
	      (zip-without-null (cdr xs) (cdr ys))))))


;; Simplifications

;; De-nil-ing is used for post-processing parsed filebuffer schemata,
;; since ESRAP teends to be verbose.

(defun denil (l)
  "Simplify a list L.

We remove NIL values, and if this results in a singleton list,
remove the level of listing."
  (labels ((remove-nil (l)
	     "Remove NIL values from L."
	     (if (null l)
		 nil

		 (destructuring-bind (head &rest tail)
		     l
		   (cond ((listp head)
			  (let ((denilled (remove-nil head)))
			    (cond ((null denilled)
				   (remove-nil tail))
				  ((= (length denilled) 1)
				   (cons (car denilled) (remove-nil tail)))
				  (t
				   (cons denilled (remove-nil tail))))))
			 ((null head)
			  (remove-nil tail))
			 (t
			  (cons head (remove-nil tail))))))))

    (remove-nil l)))
