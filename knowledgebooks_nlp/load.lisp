
(load "FastTag")

(defpackage kbnlp
  (:use :cl :asdf)
  (:export
   :make-text-object)
  (:documentation "Mark Watson's NLP utilities released under the AGPL"))

(in-package :kbnlp)

(load "KBtm")
(in-package :kbnlp)

(defun cload (path)
  (format t "\n*** starting to load ~A\n" path)
  (compile-file (format nil "~A.lisp" path))
  (load (format nil "~A.fasl" path))
  (format t "\n*** done loading ~A\n" path))


(load "data/FastTagData.lisp")
(load "data/cat-data-tables.lisp")
(load "load-misc-data")
(cload "names")
(cload "places")

(cload "cat-tags")
(cload "summarize")
(cload "to-xml")
(cload "find-names-places")
(cload "stem-text")

;;; utilities:
(cload "misc-utils")

