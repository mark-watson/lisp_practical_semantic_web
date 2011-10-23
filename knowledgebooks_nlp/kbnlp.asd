;; This file is part of KnowledgeBooks NLP Library
;;
;; Copyright (c) 2005-2010 Mark Watson
;; License: AGPL version 3 (or alternative approved by author)
;;
;; All rights reserved.

;;;; -*- Mode: LISP -*-

;; to load:  (asdf:operate 'asdf:load-op :kbnlp)

;;(in-package :cl-user)

;;(load "load.lisp")

(in-package :asdf)

(defsystem :kbnlp
  :name "kbnlp"
  :author ""
  :version "1"
  :maintainer ""
  :licence "AGPL"
  :description ""
  :long-description ""

  :components ((:file "package")
              (:file "KBtm" :depends-on ("package"))
              (:file "data/FastTagData" :depends-on ("package"))
			  (:file "FastTag" :depends-on ("package"))
              (:file "data/cat-data-tables" :depends-on ("package"))
              (:file "cat-tags" :depends-on ("package"))
			  (:file "load-misc-data" :depends-on ("package"))
			  (:file "names" :depends-on ("package"))
			  (:file "places" :depends-on ("package"))
			  (:file "find-names-places" :depends-on ("package"))
			  (:file "summarize" :depends-on ("package"))
			  (:file "misc-utils" :depends-on ("package"))))

