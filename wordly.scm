;;; This is a Wordle-type game written in Chicken Scheme
;;; The list of words comes from the system dictionary

;;; Handy stuff
;;; (import apropos-csi)
;;; (require-library chicken-doc)

(import (chicken io))
(import (chicken random))

;;; System dictionary location
(define dict-file "/usr/share/dict/words")

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

;;; Return a random word from a list of words
(define (random-word dict)
  (let ((words (list->vector dict)))
    (vector-ref words (pseudo-random-integer (vector-length words)))))

;;; Remove the first matching character from a list of character
(define (rem-char-1 l c)
  (if (char=? (car l) c)
      (cdr l)
      (cons (car l) (rem-char-1 (cdr l) c))))

;;; Play a wordle game
(define (guess-word)
  (let*
      ((dict (read-words dict-file))
       (word (random-word dict))
       (word-letters (string->list word))
       (guess "     ")
       (guess-state (make-vector word-length 'unused))
       (letters "abcdefghijklmnopqrstuvwxyz")
       (letter-state (make-vector 26 'unused)))
    (define (in-dict? word)
      (list? (member word dict)))
    (define (set-letter-state c state)
      (if (char-lower-case? c)
          (vector-set! letter-state (- (char->integer c) (char->integer #\a)) state)))
    (define (update-state)
      (let ((letters-left word-letters))
        ;; Clear down the letter states for this guess
        (do ((i 0 (+ i 1))) ((= i 26) #f)
          (let ((state (vector-ref letter-state i)))
            (cond
             ((eqv? state 'correct) (vector-set! letter-state i 'unused))
             ((eqv? state 'wrong-pos) (vector-set! letter-state i 'unused))
             (else #f))))
        ;; Pass 1, mark the correct letters and remove from letters-left list
        (do ((i 0 (+ i 1))) ((= i word-length) #f)
          (if (char=? (string-ref word i) (string-ref guess i))
              (begin
                (vector-set! guess-state i 'correct)
                (set-letter-state (string-ref guess i) 'correct)
                (set! letters-left (rem-char-1 letters-left (string-ref guess i))))
              (vector-set! guess-state i 'unused)))
        ;; Pass 2, mark the letters in the wrong position and remove from letters-left list
        (do ((i 0 (+ i 1))) ((= i word-length) #f)
          (if (eqv? (vector-ref guess-state i) 'unused)
              (if (list? (member (string-ref guess i) letters-left))
                  (begin
                    (vector-set! guess-state i 'wrong-pos)
                    (set-letter-state (string-ref guess i) 'wrong-pos)
                    (set! letters-left (rem-char-1 letters-left (string-ref guess i))))
                  (begin
                    (vector-set! guess-state i 'wrong)
                    (set-letter-state (string-ref guess i) 'wrong)))))))
    (define (print-state chars states)
      (define (print-state-attribute state)
        (cond
         ((eqv? state 'correct) (write-string "\033[32m"))
         ((eqv? state 'wrong-pos) (write-string "\033[33m"))
         ((eqv? state 'wrong) (write-string "\033[31m"))
         (else #f)))
      (define (clear-attributes)
        (write-string "\033[37m"))
      (do ((i 0 (+ i 1))) ((= i (string-length chars)) #f)
        (print-state-attribute (vector-ref states i))
        (write-char (string-ref chars i))
        (clear-attributes)))
    (define (print-guess-state)
      (print-state guess guess-state))
    (define (print-letter-state)
      (print-state letters letter-state))
    (define (next-guess)
      (write-string "Enter your word:")
      (set! guess (read-line))
      (cond
       ((string=? "?" guess)
	(write-line word)
	(next-guess))
       ((not (= word-length (string-length guess)))
        (write-line "Please enter a 5 letter word")
        (next-guess))
       ((not (in-dict? guess))
        (write-line "Not in dictionary")
        (next-guess))
       ((string=? guess word) (write-line "Success!"))
       (else
        (update-state)
        (print-guess-state) (write-char #\newline)
        (print-letter-state) (write-char #\newline)
        (next-guess))))
    (next-guess)))

(guess-word)
