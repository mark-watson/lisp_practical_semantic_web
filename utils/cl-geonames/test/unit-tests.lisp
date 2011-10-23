;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          unit-tests.lisp
;;;; Purpose:       Common Lisp Geonames API Unit Tests.
;;;; Programmer:    Nicolas Lamirault <nicolas.lamirault@gmail.com>
;;;;
;;;; This file, part of cl-geonames, is Copyright (c) 2008 by Nicolas Lamirault
;;;;
;;;; cl-geonames are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;;
;;;; *************************************************************************

(in-package :cl-geonames-test)


(defun run-cl-geonames-tests ()
  "Run the cl-geonames unit tests."
  (let ((config-file (concatenate 'string
                                  *cl-geonames-path*
                                  "etc/lift-standard.config")))
    (lift:run-tests :config config-file)))

