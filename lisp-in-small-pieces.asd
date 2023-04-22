;;;; lisp-in-small-pieces.asd

(asdf:defsystem #:lisp-in-small-pieces
  :description "Describe lisp-in-small-pieces here"
  :author ""
  :license  "Dont care"
  :version "0.0.1"
  :depends-on (:alexandria)
  :serial t
  :pathname "chapters"
  :components ((:file "package")
               (:module "chapter1"
                        :components ((:file "package")
                                     (:file "1")))))
