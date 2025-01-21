;; This doesn't work!
;; (define primary '((ID) (INTEGER)))
;; (define unary '((primary) (MINUS primary)))
;; (define factor '(unary (MUL DIV) factor))
;; (define term '((factor (PLUS MINUS) term) (OPEN term CLOSE)))

(define (error msg)
  (display msg)
  #f)

;; primary = ID | INTEGER
(define (match-primary token)
  (let ((token-type (car token)))
    (cond
     ((eq? 'ID token-type) token)
     ((eq? 'INT token-type) token)
     (else (error "Expected ID or INTEGER")))))

;; unary = primary | MINUS primary
(define (match-unary token)
  (let ((token-type (car token)))
    (cond
     ((eq? 'MINUS token-type)
      (let ((p (match-primary (get-token))))
        (if p (list token-type p)
            #f)))
     (else (match-primary token)))))

;; factor = unary | unary (MUL|DIV) factor
(define (match-factor token)
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
                  (error "Missing factor after * or /"))))
           (else
            (unget-token op-token)
            token)))
        #f)))

(define (test-match-factor)
  (unget-token '(EOF))
  (unget-token '(INT 2))
  (unget-token '(MINUS))
  (unget-token '(MUL))
  (match-factor '(ID "a")))


;; term = factor (PLUS|MINUS) term | OPEN term CLOSE



