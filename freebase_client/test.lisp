(require :aserve)
(in-package :net.aserve.client)

(push "../utils/yason/" asdf:*central-registry*)
(asdf:operate 'asdf:load-op 'yason)

(defvar mql-url "http://api.freebase.com/api/service/mqlread?query=")

(defvar *h* (make-hash-table :test #'equal))
(defvar *h2* (make-hash-table :test #'equal))
(setf (gethash "name" *h2*) "Mark Louis Watson")
(setf (gethash "type" *h2*) (make-array 0))
(setf (gethash "query" *h*) (list *h2*))

(defvar *hs*
  (with-output-to-string
    (sstrm)
    (json:encode *h* sstrm)))

(defvar *s* (concatenate 'string mql-url (net.aserve.client::uriencode-string *hs*)))

(defvar *str-results* (do-http-request *s*))

(format t "Results:~%~%~A~%~%" *str-results*)

(defvar *results* (json:parse *str-results*))

(maphash
  #'(lambda (key val) (format t "key: ~A value: ~A~%" key val))
  (car (gethash "result" *results*)))

