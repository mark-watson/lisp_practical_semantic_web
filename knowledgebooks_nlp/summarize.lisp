(in-package :kbnlp)

;;
                                        ; This function performs a simple summarization by forming a word use histogram
                                        ; and after tossing out common words (stemmed, of course), ranking sentences
                                        ; based on how frequently words are used in them.
;;

;; Copyright Mark Watson 2001-2010. All Rights Reserved.
;; License: LGPL version 3 (http://www.gnu.org/licenses/lgpl-3.0.txt)

(defun summarize (txt-obj &optional (num-return 3))
  (let* ((words (text-text txt-obj))
         (num-words (length words))
	 (cats (text-category-tags txt-obj))
         (sentence-count 0)
	 best-sentences sentence (score 0))
    ;; loop over sentences:
    (dotimes (i num-words)
      (let* ((word (svref words i)))
	(dolist (cat cats)
	  (let* ((hash (gethash (car cat) categoryToHash))
		 (value (gethash word hash)))
	    (if value
		(setq score (+ score (* 0.01 value (cadr cat)))))))
	(push word sentence)
	(if (or (equal word ".") (equal word "!") (equal word ";"))
	    (let ()
	      (setq sentence (reverse sentence))
	      (setq score (/ score (1+ (length sentence))))
	      (setq sentence-count (1+ sentence-count))
	      ;;(format t "~%~A : ~A~%" sentence score)
	      ;; process this sentence:
	      (if (and (> score 1.0) (> (length sentence) 4) (< (length sentence) 30))
		  (progn
		    (setq sentence
                          (reduce #'(lambda (x y) (concatenate 'string x " " y)) (coerce sentence 'list)))
		    (push (list sentence score) best-sentences)))
	      (setf sentence nil score 0)))))
    (setf best-sentences (sort best-sentences #'(lambda (x y) (> (cadr x) (cadr y)))))
    (if best-sentences
        (replace-all
         (reduce #'(lambda (x y) (concatenate 'string x " " y)) (mapcar #'(lambda (x) (car x)) best-sentences))
         " ." ".")
      "<no summary>")))


