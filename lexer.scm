(import (chicken io))
(import (chicken port))

(define *saved-tokens* '())

;;; Reserved words
(define reserved-words
  '(("integer" INTEGER)
    ("var" VAR)
    ("begin" BEGIN)
    ("end" END)
    ("function" FUNCTION)))

;;; Read a token from input and return it
(define (get-token-from-input)
  (let loop ((c (peek-char)))
    (cond
     ((eof-object? c) c)
     ((char-whitespace? c) (read-char) (loop (peek-char)))
     ((char-alphabetic? c) (get-identifier))
     ((char-numeric? c) (get-number))
     ((char=? c #\;) (read-char) '(SEMICOLON))
     ((char=? c #\+) (read-char) '(PLUS))
     ((char=? c #\-) (read-char) '(MINUS))
     ((char=? c #\/) (read-char) '(DIV))
     ((char=? c #\*) (read-char) '(MUL))
     ((char=? c #\=) (read-char) '(EQUAL))
     ((char=? c #\() (read-char) '(OPEN))
     ((char=? c #\)) (read-char) '(CLOSE))
     ((char=? c #\,) (read-char) '(COMMA))
     ((char=? c #\:) (read-char)
      (if (char=? #\= (peek-char))
          (begin (read-char) '(ASSIGN))
          '(COLON)))
     (else (read-char) (cons 'ERROR c)))))

(define (get-identifier)
  (let loop ((c (peek-char)) (identifier '()))
    (cond
     ((or (char-alphabetic? c) (char-numeric? c))
      (read-char)
      (loop (peek-char) (cons c identifier)))
     (else
      (let* ((id (list->string (reverse identifier)))
             (rw (assoc id reserved-words)))
        (if rw
            (cdr rw)
            (list 'ID id)))))))

(define (get-number)
  (let loop ((c (peek-char)) (number '()))
    (cond
     ((char-numeric? c)
      (read-char)
      (loop (peek-char) (cons c number)))
     (else
      (list 'INT (string->number (list->string (reverse number))))))))

(define (init-tokeniser)
  (set! *saved-tokens* '()))

(define (get-token)
  (if (null? *saved-tokens*)
      (get-token-from-input)
      (let ((t (car *saved-tokens*)))
        (set! *saved-tokens* (cdr *saved-tokens*))
        t)))

(define (unget-token t)
  (set! *saved-tokens* (cons t *saved-tokens*)))

(define (get-tokens)
  (let loop ((t (get-token)) (tokens '()))
    (if (eof-object? t)
        (reverse tokens)
        (loop (get-token) (cons t tokens)))))
  
(define (test-get-tokens input expected-result)
  (let ((result (with-input-from-string input get-tokens)))
    (if (equal? result expected-result)
        (begin
          (write-string input)
          (write-line " OK"))
        (begin
          (write-string input)
          (write-line " FAIL")
          (write result)))))

(test-get-tokens "function test(alpha, beta);var x:integer; begin x := 42+ alpha ; end;"
                 '((FUNCTION) (ID "test") (OPEN) (ID "alpha") (COMMA) (ID "beta") (CLOSE) (SEMICOLON)
                   (VAR) (ID "x") (COLON) (INTEGER) (SEMICOLON)
                   (BEGIN) (ID "x") (ASSIGN) (INT 42) (PLUS) (ID "alpha") (SEMICOLON)
                   (END) (SEMICOLON)))
