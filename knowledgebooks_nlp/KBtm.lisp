(in-package :kbnlp)

;; Copyright Mark Watson 2008-2010. All Rights Reserved.
;; License: LGPL version 3 (http://www.gnu.org/licenses/lgpl-3.0.txt)

;; data structures

(defstruct text
  url
  title
  summary
  category-tags
  human-names
  place-names
  text
  tags)

(defun make-text-object (words &key (url "") (title ""))
  (if (typep words 'string) (setq words (words-from-string words)))
  (let* ((txt-obj (make-text :text words :url url :title title))) ;;;  :classification cat)))
    (setf (text-tags txt-obj) (parse words))
    (let ((names-places (find-names-places txt-obj)))
      (setf (text-human-names txt-obj) (car names-places))
      (setf (text-place-names txt-obj) (cadr names-places)))
    (setf (text-category-tags txt-obj)
          (mapcar #'(lambda (x) (list (car x) (/ (cadr x) 1000000.0))) (get-word-list-category (text-text txt-obj))))
    (setf (text-summary txt-obj) (summarize txt-obj))
    txt-obj))
 

