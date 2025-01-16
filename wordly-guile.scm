;;; This is a Wordle-type game written in Chicken Scheme
;;; The list of words comes from the system dictionary

;;; Handy stuff
;;; (import apropos-csi)
;;; (require-library chicken-doc)

;(import (chicken io))
;(import (chicken random))

(use-modules (ice-9 rdelim))
(use-modules (ice-9 textual-ports))

(define (write-string str)
  (put-string (current-output-port) str))

;;; Load the dictionary
(load "words.scm")

;;; Return a random word from a list of words
(define (random-word dict)
  (let ((words (list->vector dict)))
    (vector-ref words (random (vector-length words)))))

;;; Remove the first matching character from a list of characters
(define (rem-char-1 l c)
  (if (char=? (car l) c)
      (cdr l)
      (cons (car l) (rem-char-1 (cdr l) c))))

;;; Play a wordle game
(define (guess-word)
  (let*
      ((word (random-word words))
       (word-letters (string->list word))
       (guess "     ")
       (guess-state (make-vector word-length 'unused))
       (letters "abcdefghijklmnopqrstuvwxyz")
       (letter-state (make-vector 26 'unused)))
    (define (in-dict? word)
      (list? (member word words)))
    (define (set-letter-state c state)
      (if (char-lower-case? c)
          (let ((i (- (char->integer c) (char->integer #\a))))
            (if (eqv? (vector-ref letter-state i) 'unused)
                (vector-set! letter-state i state)))))
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
         ((eqv? state 'correct) (write-string "\x1b[32m"))
         ((eqv? state 'wrong-pos) (write-string "\x1b[33m"))
         ((eqv? state 'wrong) (write-string "\x1b[31m"))
         (else #f)))
      (define (clear-attributes)
        (write-string "\x1b[37m"))
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
