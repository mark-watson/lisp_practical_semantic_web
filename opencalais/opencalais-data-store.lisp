;; Use the opencalais-lib.lisp utilities to create
;; an RDF data store. Assume that a AG RDF repository is open.

;; Copyright Mark Watson 2008-2010. All Rights Reserved.
;; License: LGPL version 3 (http://www.gnu.org/licenses/lgpl-3.0.txt)

(require :agraph)

(in-package :db.agraph.user )
(enable-!-reader)

(register-namespace "kb" "http:/knowledgebooks.com/rdfs#")

(let ((hash (make-hash-table :test #'equal)))
  (setf (gethash "Country" hash) !kb:containsCountry)
  (setf (gethash "Person" hash) !kb:containsPerson)
  (setf (gethash "Company" hash) !kb:containsCompany)
  (setf (gethash "ProvinceOrState" hash) !kb:containsState)
  (setf (gethash "Product" hash) !kb:containsProduct)
  (setf (gethash "Position" hash) !kb:containsProduct)
  (defun get-rdf-predicate-from-entity-type (entity-type)
    (let ((et (gethash entity-type hash)))
      (if (not et)
          (progn
            (setf et entity-type) ;; just use a string literal
            (format t
                    "Warning: entity-type ~S not defined in opencalais-data-store.lisp~%" entity-type)))
      et)))

(defun add-entities-to-rdf-store (subject-uri text)
  "subject-uri if the subject for triples that this function defines"
    (maphash
     #'(lambda (key val)
         (dolist (entity-val val)
           (add-triple
            subject-uri
            (get-rdf-predicate-from-entity-type key)
            (literal entity-val))))
     (cl-user::entities-from-opencalais-query text)))


