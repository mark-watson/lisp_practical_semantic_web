(require :agraph)
(in-package :db.agraph.user)

(create-triple-store "/tmp/rdfstore_2")
(register-namespace "rdf" "http://www.w3.org/1999/02/22-rdf-syntax-ns#")
(register-namespace "kb" "http:://knowledgebooks.com/ontology#")
(register-namespace "test_news" "http://news.yahoo.com/s/nm/20080616/ts_nm")

(enable-!-reader)

;;(load-ntriples "news2.nt")

;; overwrite *db* with a new RDFS++ inferencing enable
;; triple store (i.e., wrap existing triple store):

(apply-rdfs++-reasoner)

;; rdf:type examples:

(add-triple !kb:man !rdf:type !kb:person)
(add-triple !kb:sam !rdf:type !kb:man)

;; explicit:

(sparql:run-sparql "  
  PREFIX kb: <http:://knowledgebooks.com/ontology#>
  PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
  ASK  {  
    kb:sam rdf:type kb:man
  }"
  :results-format :boolean)

;; implicit:

(sparql:run-sparql "  
PREFIX kb: <http:://knowledgebooks.com/ontology#>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
ASK  {  
  kb:sam rdf:type kb:person
}"
:results-format :boolean)

;; note: query returns a false value; now use rdfs:subClassOf

(add-triple !kb:man !rdfs:subClassOf !kb:person)

(sparql:run-sparql "  
PREFIX kb: <http:://knowledgebooks.com/ontology#>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
ASK  {  
  kb:sam rdf:type kb:person
}"
:results-format :boolean)

;; now this query returns true.

;; inverse of example:

(add-triple !kb:Mark !kb:husband-of !kb:Carol)
(add-triple !kb:wife-of !owl:inverseOf !kb:husband-of)  

(sparql:run-sparql "  
PREFIX kb: <http:://knowledgebooks.com/ontology#>
SELECT ?y ?x WHERE { ?y kb:wife-of ?x }")

(sparql:run-sparql "  
PREFIX kb: <http:://knowledgebooks.com/ontology#>
SELECT ?y ?x WHERE { ?y kb:wife-of ?x }" :results-format :lists)

(setq *r*
 (sparql:run-sparql "  
PREFIX kb: <http:://knowledgebooks.com/ontology#>
SELECT ?y ?x WHERE { ?y kb:wife-of ?x }" :results-format :alists))

(sparql:run-sparql "  
PREFIX kb: <http:://knowledgebooks.com/ontology#>
SELECT ?y ?x WHERE { ?y kb:wife-of ?x }" :results-format :arrays)

(sparql:run-sparql "  
PREFIX kb: <http:://knowledgebooks.com/ontology#>
SELECT ?y ?x WHERE { ?y kb:wife-of ?x }" :results-format :sparql-json)

:: Same As:

(add-triple !kb:Mark !owl:sameAs !kb:test_news:Mark)  
(add-triple !kb:Mark !kb:name !"Mark Watson")
(register-namespace "test_news" "http://news.yahoo.com/s/nm/20080616/ts_nm")
(add-triple !test_news:Mark !kb:height !"6 feet 4 inches")

(sparql:run-sparql "  
PREFIX kb: <http:://knowledgebooks.com/ontology#>
SELECT ?x ?name ?height WHERE { ?x kb:height ?height .
                                ?x kb:name ?name }" :results-format :lists)

(sparql:run-sparql "  
PREFIX kb: <http:://knowledgebooks.com/ontology#>
SELECT ?p ?o WHERE { kb:Mark ?p ?o }" :results-format :lists)

;; owl:TransitiveProperty

(add-triple !kb:relativeOf !rdf:type !owl:TransitiveProperty)
(add-triple !kb:Mark !kb:relativeOf !kb:Ron)
(add-triple !kb:Ron !kb:relativeOf !kb:Julia)
(add-triple !kb:Julia !kb:relativeOf !kb:Ken)

(defvar *r2*
  (sparql:run-sparql "  
PREFIX kb: <http:://knowledgebooks.com/ontology#>
SELECT ?relative WHERE { kb:Mark kb:relativeOf ?relative }" :results-format :sparql-json))

;; OLD FROM LAST CHAPTER: REMOVE:

;; warning: the following generates an XML response:

(sparql:run-sparql "  
PREFIX kb: <http:://knowledgebooks.com/ontology#>
SELECT ?article_uri ?city_name  WHERE {  
  ?article_uri kb:containsCity ?city_name .
}")


;; generate a list of hash tables response:

(defvar *r1*
 (sparql:run-sparql "  
   PREFIX kb: <http:://knowledgebooks.com/ontology#>
   SELECT ?article_uri ?city_name  WHERE {  
          ?article_uri kb:containsCity ?city_name .
   }"
  :results-format :hashes))

(dolist (result *r1*)
  (maphash
   #'(lambda (key value)
       (format t " key: ~S value: ~S~%"
               key value))
   result))

;; try an ASK query:

(sparql:run-sparql "  
PREFIX kb: <http:://knowledgebooks.com/ontology#>
ASK  {  
  ?any_article kb:containsCity 'Chicago'
}"
:results-format :boolean)

;; query that returns a list of arrays for each result:

(sparql:run-sparql "  
PREFIX kb: <http:://knowledgebooks.com/ontology#>
SELECT ?article_uri ?city_name  WHERE {  
  ?article_uri kb:containsCity ?city_name .
}"
:results-format :arrays)

;; query to find all cities and people mentioned in each article:

(sparql:run-sparql "  
PREFIX kb: <http:://knowledgebooks.com/ontology#>
SELECT ?article_uri ?city_name ?person_name  WHERE {  
  ?article_uri kb:containsCity ?city_name .
  ?article_uri kb:containsPerson ?person_name .
}"
:results-format :arrays)

;; using regular expression filters:

(sparql:run-sparql "  
PREFIX kb: <http:://knowledgebooks.com/ontology#>
SELECT ?article_uri  WHERE {
  ?article_uri kb:containsPerson ?person_name .
  FILTER regex(?person_name, '^*Putin*')
}"
:results-format :lists)




