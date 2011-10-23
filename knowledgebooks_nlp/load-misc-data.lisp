;; initialize any data - note: this file should get loaded by (ll)

;;(defpackage names
;;  (:use 
;;   common-lisp)
;;  (:export
;;   #:get-word-vector-category
;;   #:find-names-places
;;   #:find-names
;;   #:find-places
;;   #:*name-prefix-list*
;;   #:pronoun-resolution)
;;  (:documentation "Mark Watson's named entity extraction code.  AGPL"))

;;(in-package :names)

(in-package :kbnlp)

(defvar *male* "male")
(defvar *female* "female")
(defvar *male-or-female* "male-or-female")

(defvar *base-path* "./")
(dolist (s asdf:*central-registry*)
  (if (and
        (stringp s)
        (search "knowledgebooks_nlp" s))
   (setf *base-path* s)))

(defvar *data-dir* (namestring (car (directory (concatenate 'string *base-path* "data")))))

(if (or
     (not (boundp '*first-name-hash*))
     (null *first-name-hash*))
    (let (line)
      (defvar *first-name-hash*)
      (setf *first-name-hash* (make-hash-table :test #'equal :size 4000))
      ;;(print *first-name-hash*)
      (with-open-file
          (in (concatenate 'string *data-dir* "/names/names.male"))
        (dotimes (i 50000)
          (setq line (read-line in nil nil))
          (if (null line) (return))
	  ;;(print line)
          (setf (gethash line *first-name-hash*) *male*)))
      (with-open-file
          (in (concatenate 'string *data-dir* "/names/names.female"))
        (dotimes (i 50000)
          (setq line (read-line in nil nil))
          (if (null line) (return))
          (if (null (gethash line *first-name-hash*))
              (setf (gethash line *first-name-hash*) *female*)
              (setf (gethash line *first-name-hash*) *male-or-female*))))))

          
(if (or
     (not (boundp '*last-name-hash*))
     (null *last-name-hash*))
    (let (line)
      (defvar *last-name-hash*)
      (setf *last-name-hash* (make-hash-table :test #'equal :size 4000))
      (with-open-file
          (in (concatenate 'string *data-dir* "/names/names.last"))
        (dotimes (i 5000000)
          (setq line (read-line in nil nil))
          (if (null line) (return))
          (setf (gethash line *last-name-hash*) *male*)))))

(if (or
     (not (boundp 'place-hash))
     (null place-hash))
    (progn
      (load (concatenate 'string *data-dir* "/names/PlaceData.ldat"))))

(if (or
     (not (boundp 'stop-word-hash))
     (null stop-word-hash))
    (progn
      (load (concatenate 'string *data-dir* "/names/StopWords.ldat"))))
          
(if (or
     (not (boundp '*company-name-hash*))
     (null *company-name-hash*))
    (let (line)
      (defvar *company-name-hash*)
      (setf *company-name-hash* (make-hash-table :test #'equal :size 400))
      (with-open-file
          (in (concatenate 'string *data-dir* "/names/names.companies"))
        (dotimes (i 500000)
          (setq line (read-line in nil nil))
          (if (null line) (return))
          (setf (gethash line *company-name-hash*) t)))))
          
(if (or
     (not (boundp '*product-name-hash*))
     (null *product-name-hash*))
    (let (line)
      (defvar *product-name-hash*)
      (setf *product-name-hash* (make-hash-table :test #'equal :size 400))
      (with-open-file
          (in (concatenate 'string *data-dir* "/names/names.products"))
        (dotimes (i 500000)
          (setq line (read-line in nil nil))
          (if (null line) (return))
          (setf (gethash line *product-name-hash*) t)))))


;;;;;;; utilities:


(defun string-starts-with (str test-prefix)
  (let ((len (length test-prefix))
	(ret t))
    (if (>= (length str) len)
	(dotimes (i len)
	  (if (not (equal (char str i) (char test-prefix i)))
	      (let ()
		(setq ret nil)
		(return))))
      (setq ret nil))
    ret))
