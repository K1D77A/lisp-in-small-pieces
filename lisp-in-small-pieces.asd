;;;; lisp-in-small-pieces.asd

(asdf:defsystem #:lisp-in-small-pieces
  :description "Describe lisp-in-small-pieces here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :pathname "chapters"
  :components ((:file "package")
               (:module "chapter1"
                        :components ((:file "1")))
               (:file "lisp-in-small-pieces")))
