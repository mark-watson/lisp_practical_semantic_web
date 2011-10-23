;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          package.lisp
;;;; Purpose:       Package definition file for cl-geonames
;;;; Programmer:    Nicolas Lamirault <nicolas.lamirault@gmail.com>
;;;;
;;;; This file, part of cl-geonames, is Copyright (c) 2006 by Nicolas Lamirault
;;;;
;;;; cl-geonames users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;;
;;;; *************************************************************************


(defpackage :cl-geonames
  (:use :cl)
  (:documentation "Geonames Common Lisp package")
  (:export #:geo-search
           #:geo-postal-code-search
           #:geo-placename-lookup
           #:geo-find-nearby-postal-code
           #:geo-find-nearby-postal-code-geocoding
           #:geo-postal-code-country-info
           #:geo-find-nearby-place-name
           #:geo-country-info
           #:geo-country-code
           #:geo-country-subdivision
           #:geo-elevation-srtm3
           #:geo-elevation-gtopo30
           #:geo-timezone

           ;; Conditions

           #:geonames-error
           #:geonames-request-error
           #:geonames-output-error 

           ;; Dev

           #:*debug*
   ))



