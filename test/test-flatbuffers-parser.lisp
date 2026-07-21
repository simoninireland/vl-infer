;;;; Tests of reading flatbuffer schemata
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
(in-suite vl-infer/flatbuffers)


;;; ---------- Reading ---------

(test test-flatbuffers-parse-example-1-from-file
  "Test we can parse eclectic.fbs from a file."
  (is (fb::parse-fbs-schema (load-test-file "eclectic.fbs"))))


(test test-flatbuffers-parse-example-1-from-stream
  "Test we can parse eclectic.fbs from a stream."
  (with-open-file (str (load-test-file "eclectic.fbs") :direction :input)
    (is (fb::parse-fbs-schema str))))


(test test-flatbuffers-parse-example-1-from-string
  "Test we can parse eclectic.fbs from a string."
  (with-open-file (str (load-test-file "eclectic.fbs") :direction :input)
    (let ((buf (make-string (file-length str))))
      (read-sequence buf str)
      (is (fb::parse-fbs-schema buf)))))


(test test-flatbuffers-parse-example-2
  "Test we can parse monsters.fbs."
  (with-open-file (str (load-test-file "monsters.fbs") :direction :input)
    (is (fb::parse-fbs-schema str))))


(test test-flatbuffers-parse-root-type
  "Test we can extract the root type."
  (let ((schema (fb::parse-fbs-schema (load-test-file "eclectic.fbs"))))
    (is (eql (fb::fbs-root-type schema) 'foobar))))


(test test-flatbuffers-root-type-table
  "Test we get a table as the root type when we compile the schema."
  (let* ((schema (fb::parse-fbs-schema (load-test-file "eclectic.fbs")))
	 (root-type (fb::make-schema schema)))
    (is (eql (fb::name root-type) 'foobar))))


(test test-flatbuffers-make-schema
  "Test we can parse and construct the supporting code for a schema."
  (let* ((schema (fb::parse-fbs-schema (load-test-file "eclectic.fbs"))))
    ;; this only checks that the schema compiles, not that it's correct
    (is (fb::make-schema schema))))


;;; ---------- Creating objects ----------

(defun make-object (object)
  "Make the OBJECT."
  (let ((object-code (fb::create-object nil object)))
    (eval (car object-code))))


(test test-flatbuffers-create-table
  "Test we can create a simple table."
  (let ((*object-types* (make-hash-table))
	(*object-type-vtable* (make-hash-table)))
    (let ((cl-code '(fb::table testtable ((first (:type fb::short))))))
      (make-object cl-code)

      (let ((table (fb::object-type 'testtable)))
	(is (eql (fb::name table) 'testtable))
	(is (= (length (fb::fields table)) 1 ))
	(let ((f (car (fb::fields table))))
	  (is (eql (fb::name f) 'first))
	  (is (equal (fb::lisp-binary-type f) '(unsigned-byte 16))))))))


(test test-flatbuffers-create-enum
  "Test we can create an enumeration."
  (let ((*object-types* (make-hash-table))
	(*object-type-vtable* (make-hash-table)))
    (let* ((cl-code'(fb::enum testenum byte ((first) (second (:default 6)) (third)))))
      (make-object cl-code)
      (let ((enum (fb::object-type 'testenum)))
	(is (eql (fb::name enum) 'testenum))
	(is (equal (fb::lisp-binary-type enum) '(unsigned-byte 8)))
	(is (= (length (fb::fields enum)) 3))
	(let ((fields (fb::fields enum)))
	  (is (equal (mapcar #'fb::name fields) '(first second third)))
	  (is (= (fb::value (car fields)) 0))
	  (is (= (fb::value (cadr fields)) 6))
	  (is (= (fb::value (cadDr fields)) 7)))))))


;;; ---------- Reading flatbuffers ----------

(test test-flatbuffers-binary-example-1
  "Test we can read an eclectic buffer using the LISP-BINARY functionality."
  (with-open-file (str (load-test-file "eclectic-example.fb") :direction :input :element-type '(unsigned-byte 8))
    (let* ((fields (list (make-instance 'fb::Field :name 'meal
						   :lisp-binary-type '(unsigned-byte 8))
			 (make-instance 'fb::Field :name 'density
						   :lisp-binary-type '(unsigned-byte 64)
						   :deprecated t)
			 (make-instance 'fb::Field :name 'say
						   :lisp-binary-type 'fb::fb-string)
			 (make-instance 'fb::Field :name 'height
						   :lisp-binary-type '(unsigned-byte 16)))))
      (defclass FooBar (fb::Table)
	()
	(:default-initargs :name 'FooBar :fields fields))

      (setf fb::*expecting* (list (make-instance 'FooBar))) ; root type

      (let ((fb (lisp-binary:read-binary 'fb::fb-header str)))
	(is (= (fb-table-field-foobar-height (fb::fb-table-header-body (fb::fb-header-root-object fb))) 100))

	(is (equal (fb::fb-string-str (fb-table-field-foobar-say (fb::fb-table-header-body (fb::fb-header-root-object fb)))) "Fi fi fo fum!"))))))


(test test-flatbuffers-read-eclectic
  "Test we can read an eclectic buffer by parsing the schema."
  (let* ((schema (fb::parse-fbs-schema (load-test-file "eclectic.fbs")))
	 (root-type (fb::make-schema schema)))

    (with-open-file (str (load-test-file "eclectic-example.fb") :direction :input :element-type '(unsigned-byte 8))
      (let ((fb (fb::read-fbs str root-type)))
	(is (= (fb-table-field-foobar-height (fb::fb-table-header-body (fb::fb-header-root-object fb))) 100))

	(is (equal (fb::fb-string-str (fb-table-field-foobar-say (fb::fb-table-header-body (fb::fb-header-root-object fb)))) "Fi fi fo fum!"))))))


(test test-flatbuffers-read-monsters
  "Test we can read a monsters buffer from its schema."
  (let* ((schema (fb::parse-fbs-schema (load-test-file "monsters.fbs")))
	 (root-type (fb::make-schema schema)))

    (with-open-file (str (load-test-file "monsters-example.fb") :direction :input :element-type '(unsigned-byte 8))
      (fb::read-fbs str root-type))

      )

    )
