;;;; lexer.lisp
;;
;;;; Copyright (c) 2018 Eric Lorenzana

(in-package #:godo)

(defparameter *illegal* "ILLEGAL")
(defparameter *eof* "EOF")

(defparameter *ident* "IDENT")
(defparameter *int* "INT")

(defparameter *assign* ":")
(defparameter *plus* "*")

(defparameter *comma* ",")
(defparameter *semicolon* ";")

(defparameter *lparen* "(")
(defparameter *rparen* ")")
(defparameter *lbrace* "{")
(defparameter *rbrace* "}")

(defclass token ()
  ((type :initarg :type
         :accessor token-type
         :type :string
         :initform nil)
   (literal :initarg :literal
            :accessor token-literal
            :type :string
            :initform nil)))

(defclass lexer ()
  ((input :initarg :input
          :accessor lexer-input
          :type string
          :initform nil)
   (position :initarg :position
             :accessor lexer-position
             :type fixnum
             :initform 0)
   (read-position :initarg :read-position
                  :accessor lexer-read-position
                  :type fixnum
                  :initform 0)
   (current-character :initarg :currentcharacter
                      :accessor lexer-current-character
                      :type char-code
                      :initform "")))

(defun make-lexer (&key input)
  (declare (type (string input)))
  (let ((l (make-instance 'lexer :input input)))
    (read-character l)
    l))

(defmethod read-character ((lexer lexer))
  ;; Out of bounds?
  (setf (lexer-current-character lexer)
        (if (>= (lexer-read-position lexer)
                (length (lexer-input lexer)))
            0
            (char (lexer-input lexer) (lexer-read-position lexer))))
  (setf (lexer-position lexer) (lexer-read-position lexer))
  (incf (lexer-read-position lexer)))
