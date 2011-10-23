(push "../knowledgebooks_nlp/" asdf:*central-registry*)
(asdf:operate 'asdf:load-op :kbnlp)

(eval-when (compile load eval)
  (require :aserve)
  (require :agraph))

(defpackage :user (:use :net.aserve.client :kbnlp))
(in-package :user)

(db.agraph.user::enable-!-reader) ; enable the ! reader macro

(db.agraph.user::create-triple-store "/tmp/webportal_rdf")
(db.agraph.user::register-namespace "kb" "http://knowledgebooks.com/rdfs#")
(db.agraph.user::register-freetext-predicate !kb:docTitle)
(db.agraph.user::register-freetext-predicate !kb:docText)

(db.agraph.user::load-ntriples #p"init.nt")

(defun valid-login? (user-name password)
  (format t "~%checking login: ~A ~A~%" user-name password)
  (let ((users-list (db.agraph.user::get-triples-list :p "<http://kbsportal.com#login>"
                                                      :o (db.agraph.user::literal user-name)))
        (password-list (db.agraph.user::get-triples-list :p "<http://kbsportal.com#password>"
                                                         :o (db.agraph.user::literal password))))
    (print users-list) (print password-list)
    (and
     users-list
     password-list
     (db.agraph.user::part=
      (db.agraph.user::subject (car users-list))
      (db.agraph.user::subject (car password-list))))))
;; <http://kbsportal.com/user/1> <http://kbsportal.com#login> "demo" .
;; <http://kbsportal.com/user/1> <http://kbsportal.com#password> "demo" .

	
(defun add-document (doc-uri doc-title doc-text)
  (let* ((txt-obj
          (kbnlp:make-text-object doc-text :title doc-title :url doc-uri))
         (resource (db.agraph.user::resource doc-uri)))
    (db.agraph.user::add-triple resource !rdf:type !kb:document)
    (db.agraph.user::add-triple resource !kb:docTitle  (db.agraph.user::literal doc-title))
    (db.agraph.user::add-triple resource !kb:docText (db.agraph.user::literal doc-text))
    (dolist (human-name (kbnlp::text-human-names txt-obj))
      (pprint human-name)
      (db.agraph.user::add-triple resource !kb:docPersonEntity  (db.agraph.user::literal human-name)))
    (dolist (place-name (kbnlp::text-place-names txt-obj))
      (pprint place-name)
      (db.agraph.user::add-triple resource !kb:docPlaceEntity  (db.agraph.user::literal place-name)))
    (dolist (tag (kbnlp::text-category-tags txt-obj))
      (pprint tag)
      (db.agraph.user::add-triple resource !kb:docTag
                                  (db.agraph.user::literal
                                   (format nil "~A/~A" (car tag) (cadr tag)))))))

;; (add-document "file:///test1.txt" "test 1" "John Smith went to Mexico instead of paying his taxes to the IRS")
;; (db.agraph.user::print-triples (db.agraph.user::get-triples))

(defun doc-search (search-term-string)
  "return a list of matching doc IDs"
  (db.agraph.user::freetext-get-ids search-term-string))

(defun get-doc-info (doc-id)
  (let* ((parts (db.agraph.user::get-triple-by-id doc-id))
         (subject (db.agraph.user::subject parts))
         (predicate (db.agraph.user::predicate parts))
         (object (db.agraph.user::object parts)))
    (mapcar
     #'(lambda (obj)
         (list
           (db.agraph.user::part->concise (db.agraph.user::subject obj))
           (db.agraph.user::part->concise (db.agraph.user::predicate obj))
           (db.agraph.user::part->terse (db.agraph.user::object obj))))
      (db.agraph.user::get-triples-list :s subject))))

;; (part->string u :format :terse)

(defun print-all-docs ()
  (dolist (result (db.agraph.user::get-triples-list :o !kb:document))
    (pprint result)
    (terpri)))

(defun delete-all-docs ()
  "This is just for testing, when I want to remove all test docs"
  (db.agraph.user::delete-triples :o !kb:document)
  (db.agraph.user::delete-triples :p !kb:docText)
  (db.agraph.user::delete-triples :p !kb:docTitle)
  (db.agraph.user::delete-triples :p !kb:docPersonEntity)
  (db.agraph.user::delete-triples :p !kb:docPlaceEntity)
  (db.agraph.user::delete-triples :p !kb:docTag)
  (db.agraph.user::delete-triples :p !kb:doc))
