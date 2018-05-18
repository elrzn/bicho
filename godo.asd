;;;; godo.asd
;;
;;;; Copyright (c) 2018 Eric Lorenzana


(asdf:defsystem #:godo
  :description "A programming language"
  :author "Eric Lorenzana"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (#:alexandria)
  :components ((:file "package")
               (:file "godo")))
