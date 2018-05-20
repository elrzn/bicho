(in-package #:bicho)

(defparameter *token-illegal* "ILLEGAL")
(defparameter *token-eof* "EOF")

(defparameter *token-ident* "IDENT")
(defparameter *token-int* "INT")

(defparameter *token-assign* ":")
(defparameter *token-plus* "*")

(defparameter *token-comma* ",")
(defparameter *token-semicolon* ";")

(defparameter *token-lparen* "(")
(defparameter *token-rparen* ")")
(defparameter *token-lbrace* "{")
(defparameter *token-rbrace* "}")

(defclass token ()
  ((type :initarg :type
         :accessor token-type
         :type :string                  ; consider deftype
         :initform nil)
   (literal :initarg :literal
            :accessor token-literal
            :type :string
            :initform nil)))

(defun make-token (&key type literal)
  "Create a new token instance."
  (make-instance 'token :type type :literal literal))

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
  "Create an instance of a lexer for the given input."
  (declare (type (string input)))
  (let ((l (make-instance 'lexer :input input)))
    (read-character l)
    l))

(defmethod out-of-bounds-p ((lexer lexer))
  "Check whether we have exceeded reading the lexer's input."
  (>= (lexer-read-position lexer)
      (length (lexer-input lexer))))

(defmethod read-character ((lexer lexer))
  "Consume the lexer's input, reading one character at a time."
  (setf (lexer-current-character lexer)
        (if (out-of-bounds-p lexer)
            0
            (char (lexer-input lexer) (lexer-read-position lexer))))
  (setf (lexer-position lexer) (lexer-read-position lexer))
  (incf (lexer-read-position lexer)))
