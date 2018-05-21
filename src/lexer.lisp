(in-package #:bicho)

(defdata token-type
  (token-type-literal string)
  (token-type-operator string)
  (token-type-delimiter string)
  (token-type-keyword string)
  (token-type-any string))

(defparameter *token-illegal* (token-type-any "ILLEGAL"))
(defparameter *token-eof* (token-type-any "EOF"))

(defparameter *token-ident* (token-type-literal "IDENT"))
(defparameter *token-int* (token-type-literal "INT"))

(defparameter *token-assign* (token-type-operator ":"))
(defparameter *token-plus* (token-type-operator "+"))

(defparameter *token-comma* (token-type-delimiter ","))
(defparameter *token-semicolon* (token-type-delimiter ";"))

(defparameter *token-lparen* (token-type-delimiter "("))
(defparameter *token-rparen* (token-type-delimiter ")"))
(defparameter *token-lbrace* (token-type-delimiter "{"))
(defparameter *token-rbrace* (token-type-delimiter "}"))

(defclass token ()
  ((type :initarg :type
         :accessor token-type
         :type token-type
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

(defmethod next-token ((lexer lexer))
  "Read and retrieve lexer's next token."
  (with-slots (current-character) lexer
    (let ((token (flet ((token-for-ch (type)
                          (make-token :type type
                                      :literal (string current-character))))
                   (match current-character
                     (#\( (token-for-ch *token-lparen*))
                     (#\) (token-for-ch *token-rparen*))
                     (#\+ (token-for-ch *token-minus*))
                     (#\+ (token-for-ch *token-plus*))
                     (#\, (token-for-ch *token-comma*))
                     (#\: (token-for-ch *token-assign*))
                     (#\; (token-for-ch *token-semicolon*))
                     (#\{ (token-for-ch *token-lbrace*))
                     (#\} (token-for-ch *token-rbrace*))
                     (_ (token-for-ch *token-illegal*))
                     ;; TODO handle EOF
                     ))))
      (read-character lexer)
      token)))
