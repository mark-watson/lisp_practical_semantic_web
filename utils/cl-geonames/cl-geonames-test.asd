;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          cl-geonames-test.asd
;;;; Purpose:       ASDF definition for cl-geonames-test
;;;; Programmer:    Nicolas Lamirault <nicolas.lamirault@gmail.com>
;;;;
;;;; This file, part of cl-geonames, is Copyright (c) 2008 by Nicolas Lamirault
;;;;
;;;; cl-geonames users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;;
;;;; *************************************************************************


(in-package :asdf)


(defsystem cl-geonames-test
    :name "cl-geonames-test"
    :author "Nicolas Lamirault <nicolas.lamirault@gmail.com>"
    :maintainer "Nicolas Lamirault <nicolas.lamirault@gmail.com>"
    :version "0.1"
    :licence "Lisp Lesser GNU General Public License"
    :description "Unit Tests for the Common Lisp wrapper of Geonames API."
    :depends-on (:cl-geonames :lift)
    :components
    ((:module :test
              :components
              ((:file "package")
               (:file "definition" :depends-on ("package"))
               (:file "specials" :depends-on ("package"))
               (:file "api" :depends-on ("specials" "definition"))
               (:file "unit-tests" :depends-on ("api"))
               ))))

