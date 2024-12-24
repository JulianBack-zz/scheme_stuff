(import (chicken process-context))
(import (chicken io))

;;; System dictionary location
(define default-file "/usr/share/dict/words")

;;; Output file
(define words-file "words.scm")

;;; The length of words we are using.
(define word-length 5)

;;; Does a word contain only the valid characters?
(define (valid-chars? word)
  (letrec 
      ((validate (lambda (cl)
                   (cond
                    ((null? cl) #t)
                    ((char-lower-case? (car cl)) (validate (cdr cl)))
                    (else #f)))))
  (validate (string->list word))))

;;; Is a word singular (as far as we can tell).  We assume that words
;;; ending in s are plural except for those ending with a double s.
(define (singular? word)
  (let ((len (string-length word)))
    (not (and (char=? #\s (string-ref word (- len 1)))
         (not (char=? #\s (string-ref word (- len 2))))))))

;;; Is a word valid?
(define (valid-word? word)
  (cond
   ((and (= word-length (string-length word)) (valid-chars? word) (singular? word)) #t)
   (else #f)))

;;; Read the file and return the valid words
(define (read-words filename)
  (letrec
      ((input (open-input-file filename))
       (next-word (lambda (words)
                    (let ((line (read-line input)))
                      (cond
                       ((eof-object? line) words)
                       ((valid-word? line) (next-word (cons line words)))
                       (else (next-word words)))))))
    (let ((word-list (next-word '())))
      (close-input-port input)
      word-list)))

(define (write-words filename words)
  (with-output-to-file filename
    (lambda ()
      (write-string "(define word-length ")
      (write-string (number->string word-length))
      (write-line ")")
      (write-line "")
      (write-line "(define words (quote")
      (write words)
      (write-line "))"))))

(let* ((args (command-line-arguments))
       (input-file (if (null? args) default-file (car args))))
  (write-words words-file (read-words input-file)))

