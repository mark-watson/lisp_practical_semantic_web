;; FastTag.lisp
;;
;; Conversion of KnowledgeBooks.com Java FastTag to Common Lisp
;;

;; Copyright Mark Watson 2008-2010. All Rights Reserved.
;; License: LGPL version 3 (http://www.gnu.org/licenses/lgpl-3.0.txt)

;; Note: you must either start with an image that contains
;; FastTagData.lisp or load this file separately

(in-package :kbnlp)

;;(defpackage fasttag
;;  (:use 
;;   common-lisp)
;;  (:export
;;   #:parse
;;   #:words-from-string
;;   #:tokenize-string)
;;  (:documentation "Mark Watson's fast tag tagger and a utility to tokenize text"))

;;(in-package :fasttag)

;;
                                        ; parse
                                        ;
                                        ;  input: a list of words (each a string)
                                        ;  output: a list of parts of speech
;;

(defun parse (words &aux lcw r w)
  (if (typep words 'string) (setq words (words-from-string words)))
  (let* ((len (length words))
         (ret (make-array (list len)))
         lastRet
         lastWord)
    (dotimes (k len)
      (setq w (aref words k))
      (setq r (gethash w lex-hash))
      ;; if this word is not in the hash table, try making it all lower case:
      (setq lcw (string-downcase w))
      (if (null r) (setq r (gethash lcw lex-hash)))
      (if (null r)
          (let ()
            (if (or
                 (equal lcw "(")
                 (equal lcw ")")
                 (equal lcw "[")
                 (equal lcw "]")
                 (equal lcw "{")
                 (equal lcw "}"))
                (setq r (list lcw)))))
      (if (null r)
          (setq r "NN")
        (if (listp r) (setq r (car r))))
      ;; apply transformation rules:

                                        ; rule 1: DT, {VBD, VBP, VB} --> DT, NN
      (if (equal lastRet "DT")
          (if (or
               (equal r "VBD")
               (equal r "VBP")
               (equal r "VB"))
              (setq r "NN")))

                                        ; rule 2: convert a noun to a number if a "." appears in the word
      (if (search "." w) (setq r "CD"))

                                        ; rule 3: convert a noun to a past participle if word ends with "ed"
      (if (equal (search "N" r) 0)
          (let ((i (search "ed" w :from-end t)))
            (if (equal i (- (length w) 2))
                (setq r "VBN"))))

                                        ; rule 4: convert any type to an adverb if it ends with "ly"
      (let ((i (search "ly" w :from-end t)))
        (if (equal i (- (length w) 2))
            (setq r "RB")))

                                        ; rule 5: convert a common noun (NN or NNS) to an adjective
                                        ;         if it ends with "al"
      (if (or
           (equal r "NN")
           (equal r "NNS"))
          (let ((i (search "al" w :from-end t)))
            (if (equal i (- (length w) 2))
                (setq r "RB"))))

                                        ; rule 6: convert a noun to a verb if the receeding word is "would"
      (if (equal (search "NN" r) 0)
          (if (equal lastWord "would")
              (setq r "VB")))

                                        ; rule 7: if a word has been categorized as a common noun and it
                                        ;         ends with "s", then set its type to a plural noun (NNS)
      (if (equal r "NN")
          (let ((i (search "s" w :from-end t)))
            (if (equal i (- (length w) 1))
                (setq r "NNS"))))

                                        ; rule 8: convert a common noun to a present participle verb
                                        ;         (i.e., a gerand)
      (if (equal (search "NN" r) 0)
          (let ((i (search "ing" w :from-end t)))
            (if (equal i (- (length w) 3))
                (setq r "VBG"))))

      (setq lastRet ret)
      (setq lastWord w)
      (setf (aref ret k) r))
    ret))


;; (fasttag:parse #("the" "cat" "ran"))
;; (fasttag:parse "President Bush went to China. He wanted a good trade agreement.")

(defun tokenize-string (string 
                        &key 
                        (delimiters '(#\Space #\Return #\Linefeed #\Newline #\. #\, #\; #\: #\! #\" #\'
                                      #\? #\/ #\( #\) #\- #\< #\>))
                        (discard '(#\Space #\Return #\Linefeed #\Newline #\, #\" #\' #\- #\< #\>))
                        (test (lambda (c) (find c delimiters)))
                        (start 0) (end (length string)) (omit-delimiters nil))
  (flet ((get-token (start)
           (if (< start end)
               (let* ((delimiterp (funcall test (char string start)))
                      (end-of-token (funcall (if delimiterp #'position-if-not #'position-if)
                                             test string :start start)))
                 (values (subseq string start end-of-token) end-of-token delimiterp))
             (values nil nil nil))))
    (let ((tokens nil)
          token delimiterp)
      (loop (multiple-value-setq (token start delimiterp) (get-token start))
        (unless (and delimiterp omit-delimiters)
          (let ((tok (string-trim discard token)))
            ;;(print (list "tok:" tok))
            (if (not (find tok discard))
                (if (> (length tok) 0)
                    (push tok tokens)))))
        (unless start (return-from tokenize-string (nreverse tokens)))))))

;; e.g.:
;;
;; (defun test5 (x) (not (equal x #\ )))
;; (tokens "the dog ran" #'test5 0)


(defun words-from-string (str)
  (let ((ss (tokenize-string str)))
    (make-array (list (length ss)) :initial-contents ss)))
