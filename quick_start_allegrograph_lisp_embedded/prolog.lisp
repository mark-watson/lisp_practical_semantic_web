(require :agraph)
(in-package :db.agraph.user)

(enable-!-reader) ; enable the ! reader macro

(create-triple-store "/tmp/rdfstore_prolog_1" :if-exists :supersede)

(register-namespace "kb" "http://knowledgebooks.com/ontology/#")

(load-ntriples #p"quick_start_allegrograph_lisp_embedded/sample_news.nt")

(print-triples (get-triples-list))

(select (?s ?p ?o)  
   (q- ?s ?p ?o))

(select (?news_uri ?summary)  
   (q- ?news_uri !kb:summary ?summary))

(select (?news_uri ?summary)  
   (q- ?news_uri !kb:summary ?summary)
   (q- ?news_uri !kb:storyType !kb:disaster))
