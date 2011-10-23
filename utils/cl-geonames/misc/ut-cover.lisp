;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          ut-cover.lisp
;;;; Purpose:       Unit tests coverage.
;;;; Programmer:    Nicolas Lamirault <nicolas.lamirault@gmail.com>
;;;;
;;;; This file, part of cl-geonames, is Copyright (c) 2008 by Nicolas Lamirault
;;;;
;;;; cl-geonames users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;;
;;;; *************************************************************************



(require :sb-cover)
(declaim (optimize sb-cover:store-coverage-data))
(asdf:oos 'asdf:load-op :cl-geonames-test)
(cl-geonames-test:run-cl-geonames-tests)
(sb-cover:report "/tmp/cl-geonames-coverage/")
(declaim (optimize (sb-cover:store-coverage-data 0)))

