;; demonstration of AllegroGraph RDF extensions:

(require :agraph)
(in-package :db.agraph.user)

(create-triple-store "/tmp/rdfstore_1")

;; default data store is kept in *db*
*db*

(register-namespace "kb" "http://knowledgebooks.com/rdfs#")

(resource "http://demo_news/12931")
(defvar *demo-article* (resource "http://demo_news/12931"))

(add-triple *demo-article* !rdf:type !kb:article :db *db* :g !"news-data")
(add-triple *demo-article* !kb:containsPerson !"Barack Obama" :db *db* :g !"news-data")

(add-triple *demo-article* !kb:processed !"yes" :db *db* :g !"work-flow")

;; query on optional graph value:
(print-triples (get-triples :g !"work-flow"))


(get-triple-by-id 3)
