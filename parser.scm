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
(define (match-primary token)
  (parse-trace "match-primary " token) 
  (let ((token-type (car token)))
    (cond
     ((eq? 'ID token-type) token)
     ((eq? 'INT token-type) token)
     (else (parse-error "Expected ID or INTEGER")))))

;; unary = primary | MINUS primary
(define (match-unary token)
  (parse-trace "match-unary " token) 
  (let ((token-type (car token)))
    (cond
     ((eq? 'MINUS token-type)
      (let ((p (match-primary (get-token))))
        (if p (list token-type p)
            #f)))
     (else (match-primary token)))))

(define (test-match-unary)
  (init-tokeniser)
  (unget-token '(EOF))
  (unget-token '(INT 2))
  (display (match-unary '(MINUS)))
  (newline)
  (init-tokeniser)
  (unget-token '(EOF))
  (display (match-unary '(ID "a")))
  (newline))

;; factor = unary | unary (MUL|DIV) factor
(define (match-factor token)
  (parse-trace "match-factor " token) 
  (let ((u (match-unary token)))
    (if u
        (let* ((op-token (get-token))
               (token-type (car op-token)))
          (cond
           ((or (eq? 'MUL token-type)
                (eq? 'DIV token-type))
            (let ((f (match-factor (get-token))))
              (if f
                  (list token-type u f)
                  (parse-error "Missing factor after * or /"))))
           (else
            (unget-token op-token)
            u)))
        #f)))

(define (test-match-factor)
  (init-tokeniser)
  (unget-token '(EOF))
  (unget-token '(INT 2))
  (unget-token '(MINUS))
  (unget-token '(MUL))
  (display (match-factor '(ID "a")))
  (newline)
  (init-tokeniser)
  (unget-token '(EOF))
  (unget-token '(INT 2))
  (unget-token '(MUL))
  (display (match-factor '(ID "a")))
  (newline)
  (init-tokeniser)
  (unget-token '(EOF))
  (unget-token '(INT 2))
  (unget-token '(MUL))
  (unget-token '(ID "a"))
  (display (match-factor '(MINUS)))
  (newline)
  (init-tokeniser)
  (unget-token '(EOF))
  (unget-token '(INT 2))
  (unget-token '(MUL))
  (unget-token '(ID "a"))
  (unget-token '(MINUS))
  (unget-token '(DIV))
  (display (match-factor '(ID "b")))
  (newline))
 

;; term = factor (PLUS|MINUS) term | OPEN term CLOSE



