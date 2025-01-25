;;; Simple recursive descent parser
;;;
;;; Rules for parsing functions
;;;
;;; 1. Each function takes the lexer and the current token as parameter
;;;
;;; 2. If successful the function returns the parse tree and consumes all the parsed tokens
;;;
;;; 3. It returns #f if unsuccessful.  It may have output an error message.  It does not consume
;;;    the failing token. (?)
;;;
;;; Next steps:
;;;
;;; 1. Add more operators: DIV, MOD, &, ~, OR
;;; 2. Relational operators: =, #, <, >, <=, >, >=
;;; 3. Add selectors: . notation for records, [] for arrays
;;; 4. Add function calls
;;; 5. Add statements: assignment, IF, WHILE, REPEAT, procedure call
;;; 6. Add variable declarations
;;; 7. Add procedure declarations
;;; 8. Add type declaractions
;;; 9. Add Module statement


(define (parse-error msg)
  (display msg)
  #f)

(define *parse-trace* #f)

; Returns a test function for a parse function
(define (make-tester match-function)
  (lambda (str expected)
    (define (parse lexer)
      (match-function lexer (get-token lexer)))
    (let ((result (parse (make-lexer-from-string str))))
      (display str)
      (if (equal? result expected)
          (display " OK")
          (begin
            (display " FAIL GOT")
            (newline)
            (display result)
            (newline)
            (display "EXPECTED")
            (newline)
            (display expected)))
      (newline))))

(define (parse-trace msg obj)
  (if *parse-trace*
      (begin
        (display msg)
        (display obj)
        (newline))))

;; primary = ID | INTEGER | OPEN term CLOSE
(define (match-primary lexer token)
  (parse-trace "match-primary " token) 
  (let ((token-type (car token)))
    (cond
     ((eq? 'ID token-type) token)
     ((eq? 'INT token-type) token)
     ((eq? 'OPEN (car token))
      (let ((t (match-term lexer (get-token lexer))))
        (if (and t (eq? 'CLOSE (car (get-token lexer))))
            t
            (parse-error "Expected )"))))
     (else (parse-error "Expected ID or INTEGER")))))

(define (test-match-primary)
  (define test (make-tester match-primary))
  (test "2" '(INT 2))
  (test "1232" '(INT 1232))
  (test "a" '(ID "a"))
  (test "xyzzy" '(ID "xyzzy"))
  (test "(a)" '(ID "a"))
  (test "(a+b)" '(ADD (ID "a") (ID "b")))
  (test "(a/b)" '(DIV (ID "a") (ID "b")))
  (test "(-a)" '(MINUS (ID "a"))))

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
  (define test (make-tester match-unary))
  (test "2" '(INT 2))
  (test "1232" '(INT 1232))
  (test "-45" '(MINUS (INT 45)))
  (test "a" '(ID "a"))
  (test "-b" '(MINUS (ID "b")))
  (test "-(a+b)" '(MINUS (ADD (ID "a") (ID "b")))))

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
  (define test (make-tester match-factor))
  (test "a" '(ID "a"))
  (test "2" '(INT 2))
  (test "-2" '(MINUS (INT 2)))
  (test "a * -2" '(MUL (ID "a") (MINUS (INT 2))))
  (test "a * 2" '(MUL (ID "a") (INT 2)))
  (test "-a * 2" '(MUL (MINUS (ID "a")) (INT 2)))
  (test "b / -a * 2" '(DIV (ID "b") (MUL (MINUS (ID "a")) (INT 2))))
  (test "2*(b / -a * 2)" '(MUL (INT 2) (DIV (ID "b") (MUL (MINUS (ID "a")) (INT 2)))))
  (test "(b / -a * 2)/x" '(DIV (DIV (ID "b") (MUL (MINUS (ID "a")) (INT 2))) (ID "x"))))

;; term = factor | factor (PLUS|MINUS) term
(define (match-term lexer token)
  (parse-trace "match-term" token)
  (let ((f (match-factor lexer token)))
    (if f
        (let* ((op-token (get-token lexer))
               (token-type (car op-token)))
          (cond
           ((or (eq? 'PLUS token-type)
                (eq? 'MINUS token-type))
            (let ((op (if (eq? 'PLUS token-type) 'ADD 'SUB))
                  (t (match-term lexer (get-token lexer))))
              (if t
                  (list op f t)
                  (parse-error "Missing term after + or -"))))
           (else
            (unget-token lexer op-token)
            f)))
        #f)))

(define (test-match-term)
  (define test (make-tester match-term))
  (test "a" '(ID "a"))
  (test "2" '(INT 2))
  (test "2 + 5" '(ADD (INT 2) (INT 5)))
  (test "-2" '(MINUS (INT 2)))
  (test "a * -2" '(MUL (ID "a") (MINUS (INT 2))))
  (test "a * 2" '(MUL (ID "a") (INT 2)))
  (test "-a * 2" '(MUL (MINUS (ID "a")) (INT 2)))
  (test "b / -a * 2" '(DIV (ID "b") (MUL (MINUS (ID "a")) (INT 2))))
  (test "a + 2" '(ADD (ID "a") (INT 2)))
  (test "3*a + 45" '(ADD (MUL (INT 3) (ID "a")) (INT 45)))
  (test "a/5 - 99" '(SUB (DIV (ID "a") (INT 5)) (INT 99)))
  (test "a + b + c" '(ADD (ID "a") (ADD (ID "b") (ID "c"))))
  (test "(a)" '(ID "a"))
  (test "(a+b)" '(ADD (ID "a") (ID "b")))
  (test "3*(a+b)" '(MUL (INT 3) (ADD (ID "a") (ID "b"))))
  (test "(a+b)/2" '(DIV (ADD (ID "a") (ID "b")) (INT 2)))
  (test "(a + b) * (c - d)" '(MUL (ADD (ID "a") (ID "b")) (SUB (ID "c") (ID "d")))))

(define (test-all)
  (test-match-primary)
  (test-match-unary)
  (test-match-factor)
  (test-match-term))
