(require :agraph)
(in-package :db.agraph.user)

;; Copyright Mark Watson 2001-2010. All Rights Reserved.
;; License: LGPL version 3 (http://www.gnu.org/licenses/lgpl-3.0.txt)

(enable-!-reader) ; enable the ! reader macro

(create-triple-store "/tmp/index_test")
(register-namespace "kb" "http://knowledgebooks.com/rdfs#")

(register-freetext-predicate !kb:containsPerson)

(freetext-registered-predicates) ; print out all predicates registered to have objects indexed

(resource "http://demo_news/12931")
(defvar *demo-article* (resource "http://demo_news/12931"))
*demo-article*

(add-triple *demo-article* !rdf:type !kb:article)
(add-triple *demo-article* !kb:containsPerson !"Barack Obama")
(add-triple *demo-article* !kb:containsPerson !"Bill Clinton")
(add-triple *demo-article* !kb:containsPerson !"Bill Jones")

(print (freetext-get-ids "Clinton"))

;; get results using a cursor (recommended):
(iterate-cursor (triple (freetext-get-triples '(and "Bill" "Jones")))
		(print triple))
(iterate-cursor (triple (freetext-get-triples "Bill"))
		(print triple))
(iterate-cursor (triple (freetext-get-triples '(or "Jones" "Clinton")))
		(print triple))

;; get results as a list:
(print
 (freetext-get-triples-list '(or "Bill" "Barack")))

