;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          cl-geonames.asd
;;;; Purpose:       ASDF definition for cl-geonames
;;;; Programmer:    Nicolas Lamirault <nicolas.lamirault@gmail.com>
;;;;
;;;; This file, part of cl-geonames, is Copyright (c) 2006 by Nicolas Lamirault
;;;;
;;;; cl-geonames users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;;
;;;; *************************************************************************


(in-package :asdf)


(defsystem cl-geonames
    :name "cl-geonames"
    :author "Nicolas Lamirault <nicolas.lamirault@gmail.com>"
    :maintainer "Nicolas Lamirault <nicolas.lamirault@gmail.com>"
    :version "0.1"
    :licence "Lisp Lesser GNU General Public License"
    :description "Common Lisp API of the Geonames web service."
    :depends-on (:drakma :cl-json :s-xml)
    :components
    ((:module :src
              :components
              ((:file "package")
               (:file "specials" :depends-on ("package"))
               (:file "conditions" :depends-on ("package"))
               (:file "api" :depends-on ("specials" "conditions"))))))
