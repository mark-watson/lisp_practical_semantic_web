(in-package :kbnlp)

;; access functions for determining category tags

;; Copyright Mark Watson 2008-2010. All Rights Reserved.
;; License: LGPL version 3 (http://www.gnu.org/licenses/lgpl-3.0.txt)

(defvar categoryHashtables)
(defvar categoryNames)

(defun get-cat-tag-names ()
  categoryNames)

(defun get-word-list-category (words)
  (let ((v nil)
        (x nil)
        (ss nil)
        (cat-hash nil)
        ;;(tags nil)
        (word nil)
        (len nil)
        (len2 nil))

    (defun list-sort (x)
      ;;(pprint x)
      (sort x #'(lambda (x y)
	        (> (cadr x) (cadr y)))))

    ;;(setf words (words-from-string words))
    ;;(setf tags (parse words))
    (setf len (length words))
    (setf len2 (length categoryHashtables))
    (setf v (make-array len2 :initial-element 0))
    ;;(pprint v)
    (do ((k 0 (+ k 1)))
        ((equal k len))
      (setf word (string-downcase (aref words k)))
      (do ((i 0 (+ i 1)))
          ((equal i len2))
        (setf cat-hash (nth i categoryHashtables))
        (setf x (gethash word cat-hash))
        (if x
            (setf (aref v i) (+ x (aref v i))))))
    (setf ss '())
    (do ((i 0 (+ i 1)))
        ((equal i len2))
      (if (> (aref v i) 0.01)
          (setf ss (cons (list (nth i categoryNames) (round (* (aref v i) 10))) ss))))
    (setf ss (list-sort ss))
    (let ((cutoff (/ (cadar ss) 2))
          (v '()))
      (dolist (hit ss)
        (if (> (cadr hit) cutoff)
            (setf v (cons hit v))))
      (reverse v))))


;; test:  (get-word-list-category (words-from-string "banking in Europe is a good business. The Euro is strong and account balances are up. Interest rates are remaining steady."))
