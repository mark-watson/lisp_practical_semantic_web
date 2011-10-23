;;(in-package :names)

(in-package :kbnlp)

;;#+(or xanalys lispworks)
;;(proclaim '(optimize (speed 3) (space 0) (debug 0)))

;; count number of places
(defun count-places (&aux (count 0))
  (maphash
   #'(lambda (key value) (setq count (1+ count)))
   place-hash)
  count)

;;
;; utility for detecting place names in a word list
;;
;;
;; argument: a list of words as strings
;; return: a list of lists. each sub-list contains a starting and ending index)
;;
(defun find-places (words exclusion-list)
  (let* ((len (length words))
	 (ret '())
	 word)
    (dotimes (i len)
      (setq word (aref words i))
      ;; process 3 word place names:
      (if (< i (- len 2))
	  (if (and
	       (not-in-list-find-places-helper ret i (+ i 3))
	       (not-in-list-find-places-helper exclusion-list i (+ i 3)))
	      (let ((words (concatenate 'string word " " (aref words (1+ i)) " " (aref words (+ i 2)))))
		(if (gethash words place-hash)
		    (setq ret (cons (list i (+ i 3)) ret))))))
      ;; process 2 word place names:
      (if (< i (1- len))
	  (if (and
	       (not-in-list-find-places-helper ret i (+ i 2))
	       (not-in-list-find-places-helper exclusion-list i (+ i 2)))
	      (let ((words (concatenate 'string word " " (aref words (1+ i)))))
		(if (gethash words place-hash)
		    (setq ret (cons (list i (+ i 2)) ret))))))
      ;; 1 word place names:
      (if (and
	   (not-in-list-find-places-helper ret i (+ i 1))
	   (not-in-list-find-places-helper exclusion-list i (+ i 1)))
	  (if (gethash word place-hash)
	      (setq ret (cons (list i (1+ i)) ret)))))
    ;;(print (list "debug: places:" (reverse ret)))
    (reverse ret)))

  (defun not-in-list-find-places-helper (a-list start end &aux (rval t))
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


;; test: (find-places '("President" "Bush" "went" "to" "San" "Diego"))
(defun test-places ()
  (let* ((words '#("President" "Bush" "went" "to" "France" "and" "Germany" "to" "meet" "Ms" "." "Jones")))
    (find-places words '((10 11)))))
