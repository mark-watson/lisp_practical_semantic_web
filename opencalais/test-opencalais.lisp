(require :agraph)
(in-package :db.agraph.user)

(create-triple-store "/tmp/rdfstore_1")

;;(cl-user::add-entities-to-rdf-store  !<http://newsdemo.com/1234>  "Senator Hiliary Clinton spoke with the president of France. Clinton and John Smith talked on the airplane going to California. IBM and Pepsi contributed to Clinton's campaign.")

(print-triples (get-triples-list) :format :concise)

(print-triples
 (get-triples-list
  :p (get-rdf-predicate-from-entity-type "Company")
  :o (literal "Pepsi"))
 :format :concise)
