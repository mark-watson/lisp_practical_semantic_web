(in-package :kbnlp)

;;
 ; save-text-objects-to-xml
 ;;
 
;; Copyright Mark Watson 2001-2010. All Rights Reserved.
;; License: LGPL version 3 (http://www.gnu.org/licenses/lgpl-3.0.txt)

(defun to-xml (text-obj &aux (ret "") nl)
  (setq nl (make-string 1 :initial-element #\newline))

  (defun add (s)
    (setq ret (concatenate 'string ret s nl)))
  
  (add "  <text>")
  (add (concatenate 'string "    <url>" (text-url text-obj) "</url>"))

  (add "    <classifications>")
  (let* ((v (text-classifications text-obj))
	 (len (length v))
         (top-names (text-classification-top-categories text-obj))
	 z)
    (dotimes (k len)
      (let ((cat-sub-list (nth k v))
            (cat-top-name (nth k top-names)))
        ;;(print (list cat-top-name cat-sub-list))
        (dolist (x cat-sub-list)
          (add (concatenate 'string "        <category top_cat=\"" (princ-to-string cat-top-name)
                            "\" sub_cat=\"" (princ-to-string (car x))
                            "\" score=\"" (princ-to-string (cadr x))
                            "\" />"))))))
  (add "    </classifications>")

  (add "    <key-words>")
  (let ((w (text-key-words text-obj)))
    (dolist (z w)
      (add (concatenate 'string "      <key-word score=\"" (princ-to-string (cadr z)) "\">"
                        (princ-to-string (car z)) "</key-word>"))))
  (add "    </key-words>")

  (add "    <key-phrases>")
  (let* ((v (text-key-phrases text-obj))
	 (len (length v))
	 z)
    (dotimes (k len)
      (setq z (nth k v))
      (let ((word-list (car z))
            (score (cadr z)))
        (print (list word-list score))
        (add (concatenate 'string "        <key-phrase score=\"" (princ-to-string score) "\">"))
        (dolist (w word-list)
          (add (concatenate 'string "          <word>" (princ-to-string w) "</word>")))

        (add (concatenate 'string "        </key-phrase>")))))

  (add "    </key-phrases>")

  (add "    <human-names>")
  (let ((z (text-human-names text-obj)))
    (dolist (s z)
      (add (concatenate 'string "      <human-name>" s "</human-name>")))) 
  (add "    </human-names>")
  (add "    <place-names>")


  
  (let ((z (text-place-names text-obj)))
    (dolist (s z)
      (add (concatenate 'string "      <place-name>" s "</place-name>")))) 
  (add "    </place-names>")
  (add (concatenate 'string "    <title>" (text-title text-obj) "</title>"))
  (add (concatenate 'string "    <local-file-path>"
		    (text-local-file-path text-obj) "</local-file-path>"))
  (add "    <summary>")
  (add (concatenate 'string "      " (text-summary text-obj)))
  (add "    </summary>")
  (add "    <textdata>")
  (let* ((v (text-text text-obj))
	 (tags (text-tags text-obj))
	 (stems (text-stems text-obj))
	 (len (length v))
	 z)
    (dotimes (k len)
      (setq z (aref v k))
      (add (concatenate 'string "        <word tag=\"" (aref tags k)
			"\" stem=\"" (aref stems k) "\">"
			(princ-to-string z) "</word>"))))
  (add "    </textdata>")
  (add "  </text>"))

