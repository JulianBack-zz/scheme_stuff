;;; Simple Lexer for an Oberon-style language
;;;
;;; TODO
;;;
;;; 1. DONE Support line numbering
;;; 2. DONE Use hash table for reserved words
;;; 3. DONE Store filename in lexer record
;;; 4. DONE Add additional tokens
;;; 5. DONE Comments, including nested
;;; 6. DONE Strings and characters
;;; 7. DONE Hex integers and characters
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

(define (close-lexer lexer)
  (close-input-port (lexer-get-port lexer)))

;;; Reserved words
(define *reserved-words*
  (alist->hash-table '(("var" VAR)
                       ("begin" BEGIN)
                       ("end" END)
                       ("procedure" PROCEDURE)
                       ("div" DIV)
                       ("mod" MOD)
                       ("or" OR)
                       ("if" IF)
                       ("then" THEN)
                       ("else" ELSE)
                       ("elseif" ELSEIF)
                       ("while" WHILE)
                       ("do" DO)
                       ("repeat" REPEAT)
                       ("until" UNTIL)
                       ("array" ARRAY)
                       ("of" OF)
                       ("record" RECORD)
                       ("const" CONST)
                       ("type" TYPE)
                       ("module" MODULE)
                       ("return" RETURN)
                       ("import" IMPORT))))

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
           ((char=? c #\=) (read-char port) '(EQ))
           ((char=? c #\)) (read-char port) '(CLOSE))
           ((char=? c #\,) (read-char port) '(COMMA))
           ((char=? c #\&) (read-char port) '(AND))
           ((char=? c #\#) (read-char port) '(NE))
           ((char=? c #\~) (read-char port) '(NOT))
           ((char=? c #\[) (read-char port) '(OPENSQ))
           ((char=? c #\]) (read-char port) '(CLOSESQ))
           ((char=? c #\.) (read-char port) '(DOT))
           ((char=? c #\") (read-char port) (get-string port))
           ((char=? c #\() (read-char port)
            (if (char=? #\* (peek-char port))
                (begin
                  (read-char port)
                  (if (eof-object? (skip-comment lexer))
                      '(ERROR "EOF in comment")
                      (loop (peek-char port))))
                '(OPEN)))
           ((char=? c #\:) (read-char port)
            (if (char=? #\= (peek-char port))
                (begin (read-char port) '(ASSIGN))
                '(COLON)))
           ((char=? c #\>) (read-char port)
            (if (char=? #\= (peek-char port))
                (begin (read-char port) '(GE))
                '(GT)))
           ((char=? c #\<) (read-char port)
            (if (char=? #\= (peek-char port))
                (begin (read-char port) '(LE))
                '(LT)))
           (else (read-char port) (list 'ERROR c)))))))

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
     ((and (char? c) (char-alphabetic? c))
      (read-char port)
      (loop (peek-char port) (cons c number)))
     (else
      (let ((value (cond
                    ((char-ci=? #\H (car number))
                     (list 'INT (string->number (list->string (reverse (cdr number))) 16)))
                    ((char-ci=? #\X (car number))
                     (list 'CHAR (integer->char (string->number (list->string (reverse (cdr number))) 16))))
                    (else
                     (list 'INT (string->number (list->string (reverse number))))))))
        (if (cadr value)
            value
            (list 'ERROR "Bad number")))))))

; Get a string, does not allow and escapes or newlines
(define (get-string port)
  (let loop ((c (read-char port)) (string '()))
    (if (char? c)
        (cond
         ((char=? c #\")
          (list 'STRING (list->string (reverse string))))
         ((char=? c #\newline)
          '(ERROR "Newline in string"))
         (else
          (loop (read-char port) (cons c string))))
        '(ERROR "EOF in string"))))

; Returns EOF object if end of file hit
(define (skip-comment lexer)
  (let ((port (lexer-get-port lexer)))
    (let loop ((c (peek-char port)))
      (cond
       ((eof-object? c) c)
       ((char=? c #\newline)
        (inc-line-number lexer)
	    (loop (read-char port)))
       ((char=? c #\()
        (if (char=? #\* (peek-char port))
            (begin
              (read-char port)
              (skip-comment lexer)
              (loop (peek-char port)))))
       ((char=? c #\*)
        (if (char=? #\) (peek-char port))
            (read-char port)
            (loop (peek-char port))))
       (else
        (loop (read-char port)))))))

(define *token-trace* #f)

(define (get-token lexer)
  (let ((token (if (null? (lexer-get-saved-tokens lexer))
                   (let ((t (get-token-from-port lexer)))
                     (append t (list (lexer-get-line-number lexer))))
                   (let ((t (lexer-get-saved-tokens lexer)))
                     (lexer-set-saved-tokens! lexer (cdr t))
                     (car t)))))
    (if *token-trace*
        (begin
          (display "get ")
          (write token)
          (newline)))
    token))

(define (unget-token lexer t)
  (if *token-trace*
      (begin
        (display "unget ")
        (write t)
        (newline)))
  (lexer-set-saved-tokens! lexer (cons t (lexer-get-saved-tokens lexer))))
