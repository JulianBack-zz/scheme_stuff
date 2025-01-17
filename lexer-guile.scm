(use-modules (ice-9 rdelim))
(use-modules (ice-9 textual-ports))

(define (write-string str)
  (put-string (current-output-port) str))

;;; Reserved words
(define reserved-words
  '(("integer" INTEGER)
    ("var" VAR)
    ("begin" BEGIN)
    ("end" END)
    ("function" FUNCTION)))

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
            (list 'IDENTIFIER id)))))))

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

(test-get-tokens "function test(alpha, beta);var x:integer; begin x := 42+ alpha ; end;"
                 '((FUNCTION) (IDENTIFIER "test") (OPEN) (IDENTIFIER "alpha") (COMMA) (IDENTIFIER "beta") (CLOSE) (SEMICOLON)
                   (VAR) (IDENTIFIER "x") (COLON) (INTEGER) (SEMICOLON)
                   (BEGIN) (IDENTIFIER "x") (ASSIGN) (NUMBER 42) (PLUS) (IDENTIFIER "alpha") (SEMICOLON)
                   (END) (SEMICOLON)))
