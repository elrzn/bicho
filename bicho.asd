(asdf:defsystem #:bicho
  :description "A programming language"
  :author "Eric Lorenzana"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (#:alexandria)
  :components ((:module "src"
                :components ((:file "package")
                             (:file "lexer")
                             (:file "bicho")))))