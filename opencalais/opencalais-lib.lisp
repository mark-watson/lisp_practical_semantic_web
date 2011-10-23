;; Copyright Mark Watson 2008-2010. All Rights Reserved.
;; License: LGPL version 3 (http://www.gnu.org/licenses/lgpl-3.0.txt)

;;(eval-when (compile load eval)
;;  (require :aserve))  ;; to pick up client library
  ;;(require :agraph))

(require :aserve)
(push "../utils/split-sequence/" asdf:*central-registry*)
(asdf:operate 'asdf:load-op :split-sequence)

(defvar *my-opencalais-key*  (sys::getenv "OPEN_CALAIS_KEY"))

(defvar *PARAMS* 
    (concatenate 'string
      "&paramsXML="
       (cl-user::MAKE-ESCAPED-STRING "<c:params xmlns:c=\"http://s.opencalais.com/1/pred/\" xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\"><c:processingDirectives c:contentType=\"text/txt\" c:outputFormat=\"xml/rdf\"></c:processingDirectives><c:userDirectives c:allowDistribution=\"true\" c:allowSearch=\"true\" c:externalID=\"17cabs901\" c:submitter=\"ABC\"></c:userDirectives><c:externalMetadata></c:externalMetadata></c:params>")))

(defun entities-from-opencalais-query (query &aux url results index1 index2 lines tokens hash)
  (setf hash (make-hash-table :test #'equal))
  (setf url
    (concatenate 'string
      "http://api.opencalais.com/enlighten/calais.asmx/Enlighten?"
      "licenseID="
      *my-opencalais-key*
      "&content="
      (cl-user::MAKE-ESCAPED-STRING  query)
      *PARAMS*))
  (setf results (net.aserve.client:do-http-request url))
  (setq index1 (search "terms of service.--&gt;" results))
  (setq index1 (search "&lt;!--" results :start2 index1))
  (setq index2 (search "--&gt;" results :start2 index1))
  (setq results (subseq results (+ index1 7) index2))
  (setq lines
    (split-sequence:split-sequence #\Newline results))
  (dolist (line lines)
    (setq index1 (search ": " line))
    (if index1
        (let ((key (subseq line 0 index1))
              (values (split-sequence:split-sequence ", " (subseq line (+ index1 2)))))
          (if (not (string-equal "Relations" key))
              (setf (gethash key hash) values)))))
  (maphash
   #'(lambda (key val) (format t "key: ~S val: ~S~%" key val))
   hash)
  hash)


;;  (setf x (entities-from-opencalais-query "Senator Hiliary Clinton spoke with the president of France. Clinton and John Smith talked on the airplane going to California. IBM and Pepsi contributed to Clinton's campaign."))


(defun replace-all (string part replacement &key (test #'char=))
"Returns a new string in which all the occurrences of the part 
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


