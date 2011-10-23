(require :agraph)
(in-package :db.agraph.user)

;; Copyright Mark Watson 2001-2010. All Rights Reserved.
;; License: LGPL version 3 (http://www.gnu.org/licenses/lgpl-3.0.txt)

(enable-!-reader) ; enable the ! reader macro

(create-triple-store "/tmp/rdfstore_1")
(register-namespace "kb" "http://knowledgebooks.com/rdfs#")

(resource "http://demo_news/12931")
(defvar *demo-article* (resource "http://demo_news/12931"))
*demo-article*

(add-triple *demo-article* !rdf:type !kb:article)
(add-triple *demo-article* !kb:containsPerson !"Barack Obama")

(get-triple-by-id 2)
(defvar *triple* (get-triple-by-id 2))

;; get triples as a list:
(get-triples-list :o  !"Barack Obama")

;; get triples using a cursor:
(setq a-cursor (get-triples :s *demo-article*))
(while (cursor-next-p a-cursor)
  (print (cursor-next-row a-cursor))) ; cursor-next returns a vector, not a triple

(print-triple *triple* :format :concise)
(print-triple *triple*)
(print-triples (list *triple*))

;; queries:
(print-triples (get-triples))
(print-triples (get-triples :s *demo-article*))
(print-triples 
   (get-triples :s *demo-article* :o !kb:article))

;; write triple store to a file in NTriple format:
(with-open-file
  (output "/tmp/sample.ntriple" 
    :direction :output 
    :if-does-not-exist :create) 
   (print-triples (get-triples) :stream output :format :ntriple))

