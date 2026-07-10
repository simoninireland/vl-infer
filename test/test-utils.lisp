;;;; Tests of utility functions
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

(in-package :vl-infer/test)
(in-suite vl-infer/utils)


;;; ---------- De-nilling lists ----------

(test test-utils-denil
  "Test we can de-nil lists."
  ;; empty listp
  (is (null (fb::denil nil)))

  ;; nothing to do
  (is (equal (fb::denil '(1 2 3)) '(1 2 3)))
  (is (equal (fb::denil '(1 2 (2 3))) '(1 2 (2 3))))

  ;; removals
  (is (equal (fb::denil '(1 2 nil 3)) '(1 2 3)))
  (is (equal (fb::denil '(1 2 (2 nil 3))) '(1 2 (2 3))))
  (is (equal (fb::denil '(1 nil 2 (2 3))) '(1 2 (2 3))))
  (is (equal (fb::denil '(1 2 (2 3) nil)) '(1 2 (2 3))))

  ;; collapsing empty lists
  (is (equal (fb::denil '(1 2 (nil))) '(1 2)))
  (is (equal (fb::denil '(1 2 (nil nil nil))) '(1 2)))

  ;; collapsing singleton lists
  (is (equal (fb::denil '(1 2 (2 nil))) '(1 2 2)))
  (is (equal (fb::denil '(1 2 (nil 3))) '(1 2 3)))
  (is (equal (fb::denil '(1 2 (nil 3 nil))) '(1 2 3)))
  (is (equal (fb::denil '(1 2 (nil 2 nil 3 nil))) '(1 2 (2 3))))
  (is (null (fb::denil '((nil nil) nil (nil (nil nil)))))))
