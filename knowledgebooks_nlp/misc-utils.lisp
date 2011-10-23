;; utility functions

(in-package :kbnlp)

;; Copyright Mark Watson 2008-2010. All Rights Reserved.
;; License: LGPL version 3 (http://www.gnu.org/licenses/lgpl-3.0.txt)

(defun flatten (list)
  (if (null list)
      (list)
      (if (atom (car list))
	  (cons (car list) (flatten (cdr list)))
	  (flatten (append (car list) (cdr list))))))

(defun dump-hash-table (hash)
  (defun helper-dh (key val)
    (format t "key: ~A val:~A~%" key val))
  (maphash #'helper-dh hash))
   
(defun array->list (arr &aux ret)
  (dotimes (i (length arr))
    (if (aref arr i)
      (push (aref arr i) ret)))
  (nreverse ret))


(defun replace-all (string part replacement &key (test #'char=))
"Returns a new string in which all the occurences of the part 
is replaced with replacement."
    (with-output-to-string (out)
      (loop with part-length = (length part)
            for old-pos = 0 then (+ pos part-length)
            for pos = (search part string
                              :start2 old-pos
                              :test test)
            do (write-string string out
                             :start old-pos
                             :end (or pos (length string)))
            when pos do (write-string replacement out)
            while pos))) 


(defun collect-2-lists (l1 l2)
  (loop for x in l1
        for y in l2
        collect (list x y)))

;(collect-2-lists '(1 2 3) '(the cat ran))

