;;(in-package :names)

(in-package :kbnlp)

;;;#+(or xanalys lispworks)
;;;(proclaim '(optimize (speed 3) (space 0) (debug 0)))

(defvar *name-prefix-list*
  '("Mr" "Mrs" "Ms" "Gen" "General" "Maj" "Major" "Doctor" "Vice" "President" "Lt"
    "Premier" "Senator" "Congressman" "Prince" "King" "Representative" "Sen" "St" "Dr"))

;;
;; utility for detecting names in a word list
;;
;;
;; argument: a list of words as strings
;; return: a list of lists. each sub-list contains a starting and ending index)
;;
(defun find-names (words tags exclusion-list)
  (let* ((len (length words))
	 word
	 (ret '()))
    (dotimes (i len)
      (setq word (aref words i))
      ;; process 4 word names:
      (if (< i (- len 3))
	  ;; case #1: single element from '*name-prefix-list*'
	  (if (and
	       (not-in-list-find-names-helper ret i (+ i 4))
	       (not-in-list-find-names-helper exclusion-list i (+ i 4))
	       (member word *name-prefix-list* :test #'equal)
	       (equal "." (aref words (1+ i)))
	       (gethash (aref words (+ i 2)) *first-name-hash*)
	       (gethash (aref words (+ i 3)) *last-name-hash*))
	      (if (and
		   (string-starts-with (aref tags (+ i 2)) "NN")
		   (string-starts-with (aref tags (+ i 3)) "NN"))
		  (setq ret (cons (list i (+ i 4)) ret))))
	  ;; case #1: two elements from '*name-prefix-list*'
	  (if (and
	       (not-in-list-find-names-helper ret i (+ i 4))
	       (not-in-list-find-names-helper exclusion-list i (+ i 4))
	       (member word *name-prefix-list* :test #'equal)
	       (member (aref words (1+ i)) *name-prefix-list* :test #'equal)
	       (gethash (aref words (+ i 2)) *first-name-hash*)
	       (gethash (aref words (+ i 3)) *last-name-hash*))
	      (if (and
		   (string-starts-with (aref tags (+ i 2)) "NN")
		   (string-starts-with (aref tags (+ i 3)) "NN"))
		  (setq ret (cons (list i (+ i 4)) ret)))))
      ;; process 3 word names:
      (if (< i (- len 2))
	  (if (and
	       (not-in-list-find-names-helper ret i (+ i 3))
	       (not-in-list-find-names-helper exclusion-list i (+ i 3)))
	      (if (or
		   (and
		    (member word *name-prefix-list* :test #'equal)
		    (gethash (aref words (1+ i)) *first-name-hash*)
		    (gethash (aref words (+ i 2)) *last-name-hash*)
		    (string-starts-with (aref tags (+ i 1)) "NN")
		    (string-starts-with (aref tags (+ i 2)) "NN"))
		   (and
		    (member word *name-prefix-list* :test #'equal)
		    (member (aref words (1+ i)) *name-prefix-list* :test #'equal)
		    (gethash (aref words (+ i 2)) *last-name-hash*)
		    (string-starts-with (aref tags (+ i 1)) "NN")
		    (string-starts-with (aref tags (+ i 2)) "NN"))
		   (and
		    (member word *name-prefix-list* :test #'equal)
		    (equal "." (aref words (1+ i)))
		    (gethash (aref words (+ i 2)) *last-name-hash*)
		    (string-starts-with (aref tags (+ i 2)) "NN"))
		   (and
		    (gethash word *first-name-hash*)
		    (gethash (aref words (1+ i)) *first-name-hash*)
		    (gethash (aref words (+ i 2)) *last-name-hash*)
		    (string-starts-with (aref tags i) "NN")
		    (string-starts-with (aref tags (+ i 1)) "NN")
		    (string-starts-with (aref tags (+ i 2)) "NN")))
		  (setq ret (cons (list i (+ i 3)) ret)))))
      ;; process 2 word names:
      (if (< i (1- len))
	  (if (and
	       (not-in-list-find-names-helper ret i (+ i 2))
	       (not-in-list-find-names-helper exclusion-list i (+ i 2)))
	      (if (or
		   (and
		    (member word '("Mr" "Mrs" "Ms" "Doctor" "President" "Premier") :test #'equal)
		    (string-starts-with (aref tags (1+ i)) "NN")
		    (gethash (aref words (1+ i)) *last-name-hash*))
		   (and
		    (gethash word *first-name-hash*)
		    (gethash (aref words (1+ i)) *last-name-hash*)
		    (string-starts-with (aref tags i) "NN")
		    (string-starts-with (aref tags (1+ i)) "NN")))
		  (setq ret (cons (list i (+ i 2)) ret)))))
      ;; 1 word names:
      (if (gethash word *last-name-hash*)
	  (if (and
	       (string-starts-with (aref tags i) "NN")
	       (not-in-list-find-names-helper ret i (1+ i))
	       (not-in-list-find-names-helper exclusion-list i (1+ i)))
	      (setq ret (cons (list i (1+ i)) ret)))))
    (reverse ret)))

  (defun not-in-list-find-names-helper (a-list start end &aux (rval t))
    (dolist (x a-list)
      (let ((i1 (car x))
	    (i2 (cadr x)))
	(if (or
	     (and
	      (>= start i1)
	      (<= start i2))
	     (and
	      (>= end i1)
	      (<= end i2)))
	    (setq rval nil))))
    rval)

;; test: (find-names 

(defun test-names ()
  (let* ((words '#("President" "Bush" "went" "to" "San" "Diego" "to" "meet" "Ms" "." "Jones"
		   "and" "Gen" "." "Pervez" "Musharraf" "."))
	 (tags (parse words)))
    (print tags)
    (find-names words tags nil)))