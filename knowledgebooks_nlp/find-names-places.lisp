(in-package :kbnlp)

;; Copyright Mark Watson 2008-2010. All Rights Reserved.
;; License: LGPL version 3 (http://www.gnu.org/licenses/lgpl-3.0.txt)

(defun remove-shorter-names (lst &aux (c-lst lst) (num (length lst)))
  (dotimes (i num)
    (dotimes (j num)
      (if (not (= i j))
          (let ((s-i (nth i lst))
                (s-j (nth j lst)))
            (if (and
                 (> (length s-i) (length s-j))
                 (> (length s-j) 6))
                (if (search s-j s-i)
                    (setf c-lst (remove s-j c-lst :test #'string-equal))))))))
  c-lst)
  
;;
;; wrapper for finding human names and place names in a text object
;;
(defun find-names-places (txt-object)
  (let* ((words (text-text txt-object))
         (tags (text-tags txt-object))
         (place-indices (find-places words nil))
         (name-indices (find-names words tags place-indices))
         (name-list (remove-duplicates (build-list-find-name-helper words name-indices) :test #'equal))
         (place-list (remove-duplicates (build-list-find-name-helper words place-indices) :test #'equal)))
       (let ((ret '()))
         (dolist (x name-list)
        (if (search " " x)
            (setq ret (cons x ret))))
        (setq name-list (reverse ret)))
    (list
     (remove-shorter-names name-list)
     (remove-shorter-names place-list))))

  (defun build-list-find-name-helper (v indices)
    (let ((ret '()))
      (dolist (x indices)
        (let* ((start (car x))
               (stop (cadr x))
               (str "")
               (num (- stop start)))
          (dotimes (i num)
            (if (equal i 0)
                (setq str (concatenate 'string str (aref v (+ i start)) " "))
              (if (equal i (1- num))
                  (setq str (concatenate 'string str (aref v (+ i start))))
                (setq str (concatenate 'string str (aref v (+ i start)) " ")))))
          (setq ret (cons (string-trim '(#\Space) str) ret))))
      (reverse ret)))

;; test: (find-names-places (car *all-text-objects*))

;; Test: (dolist (x *all-text-objects*) (find-names-places x))
