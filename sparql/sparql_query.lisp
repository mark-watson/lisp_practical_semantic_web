(require :agraph)
(in-package :db.agraph.user)

(create-triple-store "/tmp/rdfstore_1")
(register-namespace "kb" "http:://knowledgebooks.com/ontology#")
(register-namespace "test_news" "http://news.yahoo.com/s/nm/20080616/ts_nm")

(load-ntriples "news.nt")

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




