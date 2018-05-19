(asdf:defsystem #:godo
  :description "A programming language"
  :author "Eric Lorenzana"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (#:godo #:prove)
  :components ((:module "t"
                :components ((:file "package")
                             (:file "lexer-test")))))
