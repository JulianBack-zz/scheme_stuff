(import (chicken port))

;;; Read a token from input and return it
(define (get-token)
  (let loop ((c (peek-char)))
    (cond
     ((eof-object? c) c)
     ((char-whitespace? c) (read-char) (loop (peek-char)))
     ((char-alphabetic? c) (get-identifier))
     ((char-numeric? c) (get-number))
     ((char=? c #\;) (read-char) '(SEMICOLON))
     ((char=? c #\+) (read-char) '(PLUS))
     ((char=? c #\-) (read-char) '(MINUS))
     ((char=? c #\/) (read-char) '(DIVIDE))
     ((char=? c #\*) (read-char) '(MULTIPLY))
     ((char=? c #\=) (read-char) '(EQUAL))
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
      (list 'IDENTIFIER (list->string (reverse identifier)))))))

(define (get-number)
  (let loop ((c (peek-char)) (number '()))
    (cond
     ((char-numeric? c)
      (read-char)
      (loop (peek-char) (cons c number)))
     (else
      (list 'NUMBER (string->number (list->string (reverse number))))))))

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

(test-get-tokens "var x:integer; x := 42+ alpha ;"
                 '((IDENTIFIER "var") (IDENTIFIER "x") (COLON) (IDENTIFIER "integer") (SEMICOLON)
                   (IDENTIFIER "x") (ASSIGN) (NUMBER 42) (PLUS) (IDENTIFIER "alpha") (SEMICOLON)))
