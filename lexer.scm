(import (chicken port))

;;; Read a token from input and return it
(define (read-token)
  (let loop ((c (peek-char)))
    (cond
     ((eof-object? c) c)
     ((char-whitespace? c) (read-char) (loop (peek-char)))
     ((char-alphabetic? c) (read-identifier))
     ((char-numeric? c) (read-number))
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

(define (read-identifier)
  (let loop ((c (peek-char)) (identifier '()))
    (cond
     ((or (char-alphabetic? c) (char-numeric? c))
      (read-char)
      (loop (peek-char) (cons c identifier)))
     (else
      (list 'IDENTIFIER (list->string (reverse identifier)))))))

(define (read-number)
  (let loop ((c (peek-char)) (number '()))
    (cond
     ((char-numeric? c)
      (read-char)
      (loop (peek-char) (cons c number)))
     (else
      (list 'NUMBER (string->number (list->string (reverse number))))))))

;; Example usage:
(define (test)
  (let ((t (read-token)))
    (if (not (eof-object? t))
        (begin
          (print t)
          (test)))))

(with-input-from-string "var x:integer; x := 42+ alpha ;" test)

