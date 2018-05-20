(asdf:defsystem #:bicho-test
  :description "A programming language"
  :author "Eric Lorenzana"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (#:bicho #:prove)
  :components ((:module "t"
                :components ((:file "package")
                             (:file "lexer-test")))))
