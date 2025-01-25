;;; Simple Lexer for an Oberon-style language
;;;
;;; TODO
;;;
;;; 1. Support line numbering
;;; 2. Use hash table for reserved words
;;; 3. Store filename in lexer record
;;; 4. Add additional tokens
;;; 5. Comments, including nested
;;; 6. Strings and characters including hex and escapes
;;; 7. Hex integers
;;; 8. Floating point constants

(import (chicken io))
(import (chicken port))

(define-record-type lexer
  (make-lexer port saved-tokens line-number)
  lexer?
  (port lexer-get-port)
  (saved-tokens lexer-get-saved-tokens lexer-set-saved-tokens!)
  (line-number lexer-get-line-number lexer-set-line-number!))

(define (make-lexer-from-string str)
  (make-lexer (open-input-string str) '() 1))

(define (make-lexer-from-file file)
  (make-lexer (open-input-file file) '() 1))

;;; Reserved words
(define reserved-words
  '(("integer" INTEGER)
    ("var" VAR)
    ("begin" BEGIN)
    ("end" END)
    ("function" FUNCTION)))

;;; Read a token from port and return it
(define (get-token-from-port lexer)
  (let ((port (lexer-get-port lexer)))
    (if (port-closed? port)
        '(EOF)
        (let loop ((c (peek-char port)))
          (cond
           ((eof-object? c) (close-input-port port) '(EOF))
           ((char-whitespace? c) (read-char port) (loop (peek-char port)))
           ((char-alphabetic? c) (get-identifier port))
           ((char-numeric? c) (get-number port))
           ((char=? c #\;) (read-char port) '(SEMICOLON))
           ((char=? c #\+) (read-char port) '(PLUS))
           ((char=? c #\-) (read-char port) '(MINUS))
           ((char=? c #\/) (read-char port) '(DIV))
           ((char=? c #\*) (read-char port) '(MUL))
           ((char=? c #\=) (read-char port) '(EQUAL))
           ((char=? c #\() (read-char port) '(OPEN))
           ((char=? c #\)) (read-char port) '(CLOSE))
           ((char=? c #\,) (read-char port) '(COMMA))
           ((char=? c #\:) (read-char port)
            (if (char=? #\= (peek-char port))
                (begin (read-char port) '(ASSIGN))
                '(COLON)))
           (else (read-char port) (cons 'ERROR c)))))))

(define (get-identifier port)
  (let loop ((c (peek-char port)) (identifier '()))
    (cond
     ((and (char? c) (or (char-alphabetic? c) (char-numeric? c)))
      (read-char port)
      (loop (peek-char port) (cons c identifier)))
     (else
      (let* ((id (list->string (reverse identifier)))
             (rw (assoc id reserved-words)))
        (if rw
            (cdr rw)
            (list 'ID id)))))))

(define (get-number port)
  (let loop ((c (peek-char port)) (number '()))
    (cond
     ((and (char? c) (char-numeric? c))
      (read-char port)
      (loop (peek-char port) (cons c number)))
     (else
      (list 'INT (string->number (list->string (reverse number))))))))

(define *token-trace* #f)

(define (get-token lexer)
  (let ((token (if (null? (lexer-get-saved-tokens lexer))
                   (get-token-from-port lexer)
                   (let ((t (lexer-get-saved-tokens lexer)))
                     (lexer-set-saved-tokens! lexer (cdr t))
                     (car t)))))
    (if *token-trace*
        (display token))
    token))

(define (unget-token lexer t)
  (lexer-set-saved-tokens! lexer (cons t (lexer-get-saved-tokens lexer))))

(define (get-tokens lexer)
  (let loop ((t (get-token lexer)) (tokens '()))
    (if (eq? 'EOF (car t))
        (reverse tokens)
        (loop (get-token lexer) (cons t tokens)))))
  
(define (test-get-tokens input expected-result)
  (let ((result (get-tokens (make-lexer-from-string input))))
    (if (equal? result expected-result)
        (begin
          (write-string input)
          (write-line " OK"))
        (begin
          (write-string input)
          (write-line " FAIL")
          (write result)))))

(define (test-lexer)
  (test-get-tokens "function test(alpha, beta);var x:integer; begin x := 42+ alpha ; end;"
                   '((FUNCTION) (ID "test") (OPEN) (ID "alpha") (COMMA) (ID "beta") (CLOSE) (SEMICOLON)
                     (VAR) (ID "x") (COLON) (INTEGER) (SEMICOLON)
                     (BEGIN) (ID "x") (ASSIGN) (INT 42) (PLUS) (ID "alpha") (SEMICOLON)
                     (END) (SEMICOLON))))
