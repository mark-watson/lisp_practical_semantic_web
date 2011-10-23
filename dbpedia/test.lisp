(require :agraph)

;; from Franz documentation:

(sparql.client::run-sparql-remote "http://dbpedia.org/sparql" "  
 PREFIX dbpedia: <http://dbpedia.org/ontology/>  
 PREFIX foaf: <http://xmlns.com/foaf/0.1/>  
 PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>  
 SELECT ?person {  
   ?person dbpedia:birthPlace <http://dbpedia.org/resource/Boston> .  
 }  
 LIMIT 10" :results-format :alists)
