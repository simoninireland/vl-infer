;;;; Tests of basic functions over flatbuffers
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


;;; ---------- Conversions to and from bytes----------

(test test-flatbuffer-numbers
  "Test we can convert to and from byte arrays."
  (is (equal (fb::16-bits-little-endian #16r34fe) '(#16rfe #16r34)))
  (is (equal (fb::16-bits-little-endian #16r1034fe) '(#16rfe #16r34)))
  (is (equal (fb::32-bits-little-endian #16r34fe) '(#16rfe #16r34 0 0)))
  (is (equal (fb::32-bits-little-endian #16r10000034fe) '(#16rfe #16r34 0 0)))

  (is (equal (fb::from-little-endian #1A(#16rfe #16r34)) #16r34fe))
  (is (equal (fb::from-little-endian #1A(#16rfe #16r34 #16r23 #16r23)) #16r232334fe)))


;;; ---------- Construction ----------

(test test-flatbuffer-create
  "Test we can create a new flatbuffers."
  (let ((builder (make-instance 'fb::Builder)))
    ;; basics
    (is (not (null (fb::buffer builder))))

    ;; check the root object offset
    (is (= (fb::retrieve-long builder 0) #16r00000100))
    (is (= (fb::offset builder) #16r00000100))))


(test test-flatbuffer-reopen-empty
  "Test we can re-open one of our own flatbuffers."
  (let ((builder (make-instance 'fb::Builder)))
    (fb::store-long #16r00000200 builder 0)

    (let ((reader (make-instance 'fb::Builder :buffer (buffer builder))))
      ;; test the offset of the reopened buffer matches the root object offset
      ;; we set explicitly
      (is (= (offset reader) #16r00000200)))))


(test test-flatbuffer-file-identifier-default
  "Test we correctly retrieve the default file identifier."
  (let ((builder (make-instance 'fb::Builder)))
    (is (string-equal (fb::file-identifier builder) "VIFB"))))


(test test-flatbuffer-file-identifier
  "Test we correctly set the file identifier."
  ;; correct
  (let ((builder (make-instance 'fb::Builder :file-identifier "NOOB")))
    (is (string-equal (fb::file-identifier builder) "NOOB")))

  ;; too long, truncate
  (let ((builder (make-instance 'fb::Builder :file-identifier "NOOBIE")))
    (is (string-equal (fb::file-identifier builder) "NOOB")))

  ;; too short, pad with 0s
  (let ((builder (make-instance 'fb::Builder :file-identifier "NO")))
    (is (string-equal (fb::file-identifier builder)
		      (make-array '(4) :element-type 'character
				       :initial-contents (mapcar #'code-char (list (char-code #\N)
										   (char-code #\O)
										   0
										   0)))))))


;;; ---------- Reading and writing ----------

(test test-flatbuffer-write-short
  "Test we can write shorts."
  (let ((builder (make-instance 'fb::Builder)))
    (fb::store-short #16r12ab builder)
    (fb::store-short #16r671a builder)

    (let* ((reader (make-instance 'fb::Builder :buffer (buffer builder)))
	   (offset (offset reader)))
      (is (= (fb::retrieve-short reader) #16r12ab))
      (is (= (fb::retrieve-short reader) #16r671a))
      (is (= (offset reader) (+ offset (* 2 2)))))))


(test test-flatbuffer-write-integer
  "Test we can write integers."
  (let ((builder (make-instance 'fb::Builder)))
    (fb::store-integer #16r12ab6754 builder)
    (fb::store-integer #16r671a330f builder)

    (let* ((reader (make-instance 'fb::Builder :buffer (buffer builder)))
	   (offset (offset reader)))
      (is (= (fb::retrieve-integer reader) #16r12ab6754))
      (is (= (fb::retrieve-integer reader) #16r671a330f))
      (is (= (offset reader) (+ offset (* 2 4)))))))


(test test-flatbuffer-write-long
  "Test we can write longs."
  (let ((builder (make-instance 'fb::Builder)))
    (fb::store-long #16r12ab675467e8939f builder)
    (fb::store-long #16r671a330f001234ab builder)

    (let* ((reader (make-instance 'fb::Builder :buffer (buffer builder)))
	   (offset (offset reader)))
      (is (= (fb::retrieve-long reader) #16r12ab675467e8939f))
      (is (= (fb::retrieve-long reader) #16r671a330f001234ab))
      (is (= (offset reader) (+ offset (* 2 8)))))))


(test test-flatbuffer-write-string
  "Test we can write and the read a string."
  (let ((builder (make-instance 'fb::Builder)))
    (fb::store-string "Hello world" builder)
    (fb::store-strings-table builder)

    (let ((reader (make-instance 'fb::Builder :buffer (buffer builder))))
      (is (string-equal (fb::retrieve-string reader) "Hello world")))))


(test test-flatbuffer-write-duplicate-strings
  "Test we share the same strings written twice."
  (let ((builder (make-instance 'fb::Builder)))
    (fb::store-string "Hello world" builder)
    (fb::store-string "Hello world" builder)
    (fb::store-strings-table builder)

    (let ((reader (make-instance 'fb::Builder :buffer (buffer builder))))
      (let ((o1 (fb::retrieve-integer reader))
	    (o2 (fb::retrieve-integer reader)))

	;; check the offsets
	(is (= o1 (+ o2 4))) ; first offset must jump the second string's offset

	;; check the strings are the same when read
	(fb::select-root-object reader)
	(is (string-equal (fb::retrieve-string reader) (fb::retrieve-string reader)))))))


(defvar FooBar (make-instance 'Table :name 'FooBar
				     :fields (list (make-instance 'Field :name 'meal
									 :lisp-binary-type '(signed-byte 8))
						   (make-instance 'Field :name 'density
									 :lisp-binary-type '(signed-byte 64)
									 :deprecated t)
						   (make-instance 'Field :name 'say
									 :lisp-binary-type 'string)
						   (make-instance 'Field :name 'height
									 :lisp-binary-type '(signed-byte 16)))))




(lisp-binary::with-open-binary-file (str "eclectic-example.fb")
  (setf fb::*expecting* (list FooBar))
  (let ((fb (lisp-binary:read-binary 'fb::fb-header str)))

    fb

    ))


(lisp-binary::with-open-binary-file (str "eclectic-example.fb")
  (let* ((n (file-length str))
	 (buf (make-array (list n) :element-type '(unsigned-byte 8))))
    (read-sequence buf str)

    (let ((str (flexi-streams:make-in-memory-input-stream buf)))

      (setf fb::*expecting* (list FooBar))
      (let ((fb (lisp-binary:read-binary 'fb::fb-header str)))

	fb

	))))
