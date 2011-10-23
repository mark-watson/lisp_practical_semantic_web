(eval-when (compile load eval)
  (require :aserve)
  (require :webactions)
  (load "backend.lisp")
  (load "../utils/file-utils.lisp"))

(defpackage :user (:use :net.aserve :net.html.generator))
(in-package :user)

		
(defun action-check-login (req ent)
  (let ((session (websession-from-req req)))
    (let ((user (websession-variable session "username")))
      (if* (and user passwd (valid-login? user passwd))
	 then ; already logged in
	      ; just go to the real home
	      "browser"
	 else ; must login
	      "login"))))

(defun action-got-login (req ent)
  (let ((session (websession-from-req req)))
    (let ((user  (cdr (assoc "username" (net.aserve:request-query req) :test #'equal)))
          (passwd (cdr (assoc "password" (net.aserve:request-query req) :test #'equal))))
      (format t "~%* *   checking login: ~A ~A~%" user passwd)
      (if* (and user passwd (valid-login? user passwd))
           then ; already logged in
           "browser"  ; just go to the real home
           else ; must login
           "login"))))

(webaction-project "dojotest"
		   :destination "web/"
		   :index "login"
		   :map
		   '(("menu"   action-check-login)
		     ("menu"    "menu.clp")
		     ("browser" "browser.clp")
		     ("search" "/do-search")
		     ("admin" "/upload.clp")
		     ("about" "/about.clp")
		     ("wiki" "/wiki.clp")
		     ("upload" "/upload.clp")
		     ;;("upload" "/do-upload" "/do-upload" (:redirect t))
		     ("gotlogin" action-got-login)
		     ("login"  "login.clp")))
		
(defun do-search (req ent)
  (net.aserve:with-http-response (req ent)
    (net.aserve:with-http-body (req ent)
      (let ((test-input (cdr (assoc "input_test_form_text" (net.aserve:request-query req)
				    :test #'equal))))
	(princ (format nil "AJAX: ~A <a href=\"/search\" target=\"new\">click here</a>" test-input) net.html.generator:*html-stream*)))))

(defparameter *known-form-items* '("description" "tags" "file"))
(defparameter *text-output-limit* 1024)

(defparameter *attachments* (make-array 10 :fill-pointer 0 :adjustable t))

;;; create a size length buffer and retrieve multipart body into it from 
;;; request. if length is nil, then retrieve the entire sequence.
;;; returns multiple values: buffer containing sequence and bytes read.
(defun fetch-multipart-sequence (req &key (length nil) (format :binary))
  (if* length
     then (let ((buffer (make-array length :element-type (if (equal format :text)
							     'character
							   '(unsigned-byte 8))))
		(start 0)
		(end length))
	    (do* ((bytes-read start index)
		  (index start (get-multipart-sequence req buffer :start index :end end)))
		((or (null index) (= index end)) (values buffer (or index bytes-read)))))
     else (let ((buffer (get-all-multipart-data req :type format)))
	    (values buffer (length buffer)))))

(defun process-upload-form (req ent &aux text-data fname)
  (with-http-response (req ent)
    (with-http-body (req ent)
      (html
       (:html
	(:head (:title "file Upload Results"))
	(:body
	 (do ((header (get-multipart-header req) (get-multipart-header req)))
	     (nil)
	   (multiple-value-bind (type item-name filename content-type)
	       (parse-multipart-header header)
	     (when filename (setf fname filename))
	     ;;(print (list "filename" filename "type:" type))
	     (when (equal type :eof) (return t)) ;; no more headers.
	     (when (member item-name *known-form-items* :test #'equal)
	       ;; it's a form item we know about, handle it.
	       (case type
		 ((:data) 
		  (push (format nil "~A" (fetch-multipart-sequence req :format :text)) text-data))
		 ((:file)
		  (let* ((mp-data (fetch-multipart-sequence req))
                         pos)
		    (setf (websession-variable (websession-from-req req) "upload-status") "Upload complete.")
		    ;; write mp-data to a file:
                     (with-open-file (ostr fname :direction :output
                                           :if-exists :supersede)
		      (write-sequence mp-data ostr :end pos)))))))))))
      ;; TBD: add text describing the semantics of the uploaded document:   <div style="padding: 30px">
      (princ (format nil "<div style=\"padding: 30px\"><div style=\"border: 2px solid black; padding: 50px; width: 500px \">")
	     net.html.generator:*html-stream*)
      (princ (format nil "<h2>File ~A uploaded OK</h2>" fname)
	     net.html.generator:*html-stream*)
      (princ (format nil "TBD: list semantic information from uploaded document here:<br/><br />")
	     net.html.generator:*html-stream*)
      (princ (format nil "<a href=\"/admin\">Continue working...</a> <br /><br />")
	     net.html.generator:*html-stream*)
      (princ (format nil "</div></div>")
	     net.html.generator:*html-stream*)))
  (format t "saved filename: ~A tags: ~A description: ~A~%" fname (car text-data) (cadr text-data))
  ;; save data to database:
  (let ((text (file-string fname)))
    (add-document (concatenate 'string "file:///" fname) fname text)))

(defparameter *response-method-not-allowed* 
    (net.aserve::make-resp 405 "Method Not Allowed"))

(push *response-method-not-allowed* net.aserve::*responses*)

(net.aserve:publish :path "/search"	   :content-type "text/html"
			        :function #'do-search)

(net.aserve:publish :path "/do-upload"
		     :content-type "text/html"
	 :function
	 #'process-upload-form)

;;;;;;;;  CLP macro calls to define custom tags:

(def-clp-function search_results (req ent args body)
  (let ((session (websession-from-req req))
        (test-input (cdr (assoc "input_test_form_text" (net.aserve:request-query req)
				    :test #'equal))))
    (push test-input (websession-variable session "history"))
    (net.html.generator:html 
     (:princ
      (format nil "Search results: ~A" test-input)))
    (if test-input
        (dolist (doc-id (doc-search test-input))
          (princ (format nil "<pre>~A</pre>~%" (get-doc-info doc-id)) net.html.generator:*html-stream*)))))

(net.aserve:start :port 8000)

;; to stop:

;; (net.aserve:shutdown)