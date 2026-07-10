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
  (is (fb::parse-fbs-schema "eclectic.fbs")))


(test test-flatbuffers-parse-example-1-from-stream
  "Test we can parse eclectic.fbs from a stream."
  (with-open-file (str "eclectic.fbs" :direction :input)
    (is (fb::parse-fbs-schema str))))


(test test-flatbuffers-parse-example-1-from-string
  "Test we can parse eclectic.fbs from a string."
  (with-open-file (str "eclectic.fbs" :direction :input)
    (let ((buf (make-string (file-length str))))
      (read-sequence buf str)
      (is (fb::parse-fbs-schema buf)))))


(test test-flatbuffers-parse-example-2
  "Test we can parse monsters.fbs."
  (with-open-file (str "monsters.fbs" :direction :input)
    (is (fb::parse-fbs-schema str))))
