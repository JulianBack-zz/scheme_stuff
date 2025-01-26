;;; Simple Lexer for an Oberon-style language
;;;
;;; TODO
;;;
;;; 1. DONE Support line numbering
;;; 2. DONE Use hash table for reserved words
;;; 3. DONE Store filename in lexer record
;;; 4. Add additional tokens
;;; 5. Comments, including nested
;;; 6. Strings and characters including hex and escapes
;;; 7. Hex integers
;;; 8. Floating point constants

(import (chicken io))
(import (chicken port))
(import srfi-69)

(define-record-type lexer
  (make-lexer port saved-tokens line-number filename)
  lexer?
  (port lexer-get-port)
  (saved-tokens lexer-get-saved-tokens lexer-set-saved-tokens!)
  (line-number lexer-get-line-number lexer-set-line-number!)
  (filename lexer-get-filename))

(define (inc-line-number lexer)
  (lexer-set-line-number! lexer (+ 1 (lexer-get-line-number lexer))))

(define (make-lexer-from-string str)
  (make-lexer (open-input-string str) '() 1 "(from string)"))

(define (make-lexer-from-file filename)
  (make-lexer (open-input-file filename) '() 1 filename))

;;; Reserved words
(define *reserved-words*
  (alist->hash-table '(("integer" INTEGER)
                       ("var" VAR)
                       ("begin" BEGIN)
                       ("end" END)
                       ("function" FUNCTION))))

;;; Read a token from port and return it
(define (get-token-from-port lexer)
  (let ((port (lexer-get-port lexer)))
    (if (port-closed? port)
        '(EOF)
        (let loop ((c (peek-char port)))
          (cond
           ((eof-object? c) (close-input-port port) '(EOF))
           ((char=? c #\newline) (inc-line-number lexer)
	        (read-char port) (loop (peek-char port)))
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
             (rw (hash-table-ref/default *reserved-words* id #f)))
        (if rw
            rw
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
                   (let ((t (get-token-from-port lexer)))
                     (append t (list (lexer-get-line-number lexer))))
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
  (let* ((lexer (make-lexer-from-string input))
	 (result (get-tokens lexer)))
    (if (equal? result expected-result)
        (begin
          (write-string input)
          (write-line " OK")
	  (write-string "Lines: ")
	  (display (lexer-get-line-number lexer))
	  (newline))
        (begin
          (write-string input)
          (write-line " FAIL")
          (write result)
          (newline)))))

(define (test-lexer)
  (test-get-tokens "function test(alpha, beta);\nvar x:integer;\nbegin\n x := 42+ alpha ;\nend;\n"
                   '((FUNCTION 1) (ID "test" 1) (OPEN 1) (ID "alpha" 1) (COMMA 1) (ID "beta" 1) (CLOSE 1) (SEMICOLON 1)
                     (VAR 2) (ID "x" 2) (COLON 2) (INTEGER 2) (SEMICOLON 2)
                     (BEGIN 3)
                     (ID "x" 4) (ASSIGN 4) (INT 42 4) (PLUS 4) (ID "alpha" 4) (SEMICOLON 4)
                     (END 5) (SEMICOLON 5))))
