;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          conditions.lisp
;;;; Purpose:       cl-geonames conditions.
;;;; Programmer:    Nicolas Lamirault <nicolas.lamirault@gmail.com>
;;;;
;;;; This file, part of cl-geonames, is Copyright (c) 2007 by Nicolas Lamirault
;;;;
;;;; cl-geonames users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;;
;;;; *************************************************************************


(in-package :cl-geonames)



(define-condition geonames-error (simple-error)
  ()
  (:documentation "Main Geonames error."))


(define-condition geonames-query-error (geonames-error)
  ((message :reader message
            :initarg :message
            :documentation "Explanation message."))
  (:documentation "Geonames query error.")
  (:report (lambda (condition stream)
             (format stream "Geonames error ~A." (message condition)))))


(define-condition geonames-request-error (geonames-error)
  ((code :reader code
         :initarg :code
         :documentation "The error code.")
   (message :reader message
            :initarg :message
            :documentation "Explanation message."))
  (:documentation "Geonames request error.")
  (:report (lambda (condition stream)
             (format stream "Geonames error ~A : ~A."
                     (code condition) (message condition)))))


(define-condition geonames-output-error (geonames-error)
  ((output :reader output
           :initarg :output
           :documentation "The output format."))
  (:documentation "Geonames output error.")
  (:report (lambda (condition stream)
             (format stream "Geonames unknown output ~A." (output condition)))))

