(load "../utils/lisp-unit.lisp")
(use-package :lisp-unit)
(load "backend.lisp")

(lisp-unit:define-test "create-doc-test1"
  (add-document "file:///test1.doc" "test title" "John Smith went to Mexico")
  (pprint (db.agraph.user::print-triples (db.agraph.user::get-triples)))
  (let ((person-list (db.agraph.user::get-triples-list :p !kb:docPersonEntity))
        (place-list (db.agraph.user::get-triples-list :p !kb:docPlaceEntity)))
    (lisp-unit:assert-equal
     (db.agraph.user::part->string
      (db.agraph.user::object (car person-list)))
     "\"John Smith\"")
    (lisp-unit:assert-equal
     (db.agraph.user::part->string
      (db.agraph.user::object (car place-list)))
     "\"Mexico\"")))

(lisp-unit:define-test "print-triples"
  (print-all-docs)
  (lisp-unit:assert-equal t t))

(lisp-unit:define-test "good-login"
  (lisp-unit:assert-equal t (valid-login? "demo" "demo")))

(lisp-unit:define-test "bad-login"
  (lisp-unit:assert-equal nil (valid-login? "demo" "demo2")))

(run-tests)

(db.agraph.user::delete-triples :o !kb:document)
(db.agraph.user::delete-triples :p !kb:docText)
(db.agraph.user::delete-triples :p !kb:docTitle)
(db.agraph.user::delete-triples :p !kb:docPersonEntity)
(db.agraph.user::delete-triples :p !kb:docPlaceEntity)
(db.agraph.user::delete-triples :p !kb:docTag)
(db.agraph.user::delete-triples :p !kb:doc)
