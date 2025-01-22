;; This doesn't work!
;; (define primary '((ID) (INTEGER)))
;; (define unary '((primary) (MINUS primary)))
;; (define factor '(unary (MUL DIV) factor))
;; (define term '((factor (PLUS MINUS) term) (OPEN term CLOSE)))

(define (parse-error msg)
  (display msg)
  #f)

(define (parse-trace msg obj)
  (display msg)
  (display obj)
  (newline))

;; primary = ID | INTEGER
(define (match-primary lexer token)
  (parse-trace "match-primary " token) 
  (let ((token-type (car token)))
    (cond
     ((eq? 'ID token-type) token)
     ((eq? 'INT token-type) token)
     (else (parse-error "Expected ID or INTEGER")))))

;; unary = primary | MINUS primary
(define (match-unary lexer token)
  (parse-trace "match-unary " token) 
  (let ((token-type (car token)))
    (cond
     ((eq? 'MINUS token-type)
      (let ((p (match-primary lexer (get-token lexer))))
        (if p (list token-type p)
            #f)))
     (else (match-primary lexer token)))))

(define (test-match-unary)
  (define (parse-unary lexer)
    (match-unary lexer (get-token lexer)))
  (define (test str)
    (display str)
    (newline)
    (display (parse-unary (make-lexer-from-string str)))
    (newline))
  (test "2")
  (test "1232")
  (test "-45")
  (test "a")
  (test "-b"))

;; factor = unary | unary (MUL|DIV) factor
(define (match-factor lexer token)
  (parse-trace "match-factor " token) 
  (let ((u (match-unary lexer token)))
    (if u
        (let* ((op-token (get-token lexer))
               (token-type (car op-token)))
          (cond
           ((or (eq? 'MUL token-type)
                (eq? 'DIV token-type))
            (let ((f (match-factor lexer (get-token lexer))))
              (if f
                  (list token-type u f)
                  (parse-error "Missing factor after * or /"))))
           (else
            (unget-token lexer op-token)
            u)))
        #f)))

(define (test-match-factor)
  (define (parse-factor lexer)
    (match-factor lexer (get-token lexer)))
  (define (test str)
    (display str)
    (newline)
    (display (parse-factor (make-lexer-from-string str)))
    (newline))
  (test "a * -2")
  (test "a * 2")
  (test "-a * 2")
  (test "b / -a * 2"))

;; term = factor (PLUS|MINUS) term | OPEN term CLOSE



