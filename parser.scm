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
;;; 1. DONE Add more operators: DIV, MOD, &, ~, OR
;;; 2. DONE Relational operators: =, #, <, >, <=, >, >=
;;; 3. DONE Add selectors: . notation for records, [] for arrays
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

;; desig = . ID | [ expr ]
(define (match-desig lexer token)
  (parse-trace "match-desig" token)
  (cond
   ((eq? 'DOT (car token))
    (let ((t (get-token lexer)))
      (if (eq? 'ID (car t))
          (list 'FIELD t)
          (parse-error "Bad field"))))
   ((eq? 'OPENSQ (car token))
    (let ((e (match-expr lexer (get-token lexer))))
      (if (and e (eq? 'CLOSESQ (car (get-token lexer))))
          (list 'INDEX e)
          (parse-error "Expected ]"))))
   (else 
    (unget-token lexer token)
    #f)))

;; location = ID {desig}

(define (match-location lexer token)
  (parse-trace "match-location" token)
  (if (eq? 'ID (car token))
      (let loop ((d (match-desig lexer (get-token lexer))) (des token))
        (cond
         ((not d)
          des)
         ((eq? 'FIELD (car d))
          (loop (match-desig lexer (get-token lexer)) (cons (car d) (cons des (cdr d)))))
         ((eq? 'INDEX (car d))
          (loop (match-desig lexer (get-token lexer)) (cons (car d) (cons des (cdr d)))))))
      #f))

(define (test-match-location)
  (define test (make-tester match-location))
  (test "a" '(ID "a" 1))
  (test "a.b" '(FIELD (ID "a" 1) (ID "b" 1)))
  (test "a.b.c" '(FIELD (FIELD (ID "a" 1) (ID "b" 1)) (ID "c" 1)))
  (test "a[5]" '(INDEX (ID "a" 1) (INT 5 1)))
  (test "a[x+1]" '(INDEX (ID "a" 1) (ADD (ID "x" 1) (INT 1 1))))
  (test "a[x+1][y]" '(INDEX (INDEX (ID "a" 1) (ADD (ID "x" 1) (INT 1 1))) (ID "y" 1)))
  (test "a[x+1].b" '(FIELD (INDEX (ID "a" 1) (ADD (ID "x" 1) (INT 1 1))) (ID "b" 1)))
  (test "a.b[4].c" '(FIELD (INDEX (FIELD (ID "a" 1) (ID "b" 1)) (INT 4 1)) (ID "c" 1))))

;; primary = ID | INTEGER | OPEN expr CLOSE
(define (match-primary lexer token)
  (parse-trace "match-primary " token) 
  (let ((l (match-location lexer token)))
    (if l
        l
        (let ((token-type (car token)))
          (cond
           ((eq? 'INT token-type) token)
           ((eq? 'OPEN (car token))
            (let ((t (match-expr lexer (get-token lexer))))
              (if (and t (eq? 'CLOSE (car (get-token lexer))))
                  t
                  (parse-error "Expected )"))))
           (else (parse-error "Expected ID or INTEGER")))))))

(define (test-match-primary)
  (define test (make-tester match-primary))
  (test "2" '(INT 2 1))
  (test "1232" '(INT 1232 1))
  (test "a" '(ID "a" 1))
  (test "xyzzy" '(ID "xyzzy" 1))
  (test "(a)" '(ID "a" 1))
  (test "(a+b)" '(ADD (ID "a" 1) (ID "b" 1)))
  (test "(a/b)" '(DIV (ID "a" 1) (ID "b" 1)))
  (test "(-a)" '(MINUS (ID "a" 1))))

;; unary = primary | MINUS primary
(define (match-unary lexer token)
  (parse-trace "match-unary " token) 
  (let ((token-type (car token)))
    (cond
     ((or (eq? 'MINUS token-type)
          (eq? 'NOT token-type))
      (let ((p (match-primary lexer (get-token lexer))))
        (if p (list token-type p)
            #f)))
     (else (match-primary lexer token)))))

(define (test-match-unary)
  (define test (make-tester match-unary))
  (test "2" '(INT 2 1))
  (test "1232" '(INT 1232 1))
  (test "-45" '(MINUS (INT 45 1)))
  (test "a" '(ID "a" 1))
  (test "-b" '(MINUS (ID "b" 1)))
  (test "-(a+b)" '(MINUS (ADD (ID "a" 1) (ID "b" 1)))))

;; factor = unary | unary (MUL|DIV) factor
(define (match-factor lexer token)
  (parse-trace "match-factor " token) 
  (let ((u (match-unary lexer token)))
    (if u
        (let* ((op-token (get-token lexer))
               (token-type (car op-token)))
          (cond
           ((or (eq? 'MUL token-type)
                (eq? 'DIV token-type)
                (eq? 'MOD token-type)
                (eq? 'AND token-type))
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
  (test "a" '(ID "a" 1))
  (test "2" '(INT 2 1))
  (test "-2" '(MINUS (INT 2 1)))
  (test "a * -2" '(MUL (ID "a" 1) (MINUS (INT 2 1))))
  (test "a * 2" '(MUL (ID "a" 1) (INT 2 1)))
  (test "-a * 2" '(MUL (MINUS (ID "a" 1)) (INT 2 1)))
  (test "b / -a * 2" '(DIV (ID "b" 1) (MUL (MINUS (ID "a" 1)) (INT 2 1))))
  (test "2*(b / -a * 2)" '(MUL (INT 2 1) (DIV (ID "b" 1) (MUL (MINUS (ID "a" 1)) (INT 2 1)))))
  (test "(b / -a * 2)/x" '(DIV (DIV (ID "b" 1) (MUL (MINUS (ID "a" 1)) (INT 2 1))) (ID "x" 1))))

;; term = factor | factor (PLUS|MINUS|OR) term
(define (match-term lexer token)
  (parse-trace "match-term" token)
  (let ((f (match-factor lexer token)))
    (if f
        (let* ((op-token (get-token lexer))
               (token-type (car op-token)))
          (cond
           ((or (eq? 'PLUS token-type)
                (eq? 'MINUS token-type)
                (eq? 'OR token-type))
            (let ((op (cond
                       ((eq? 'PLUS token-type) 'ADD)
                       ((eq? 'MINUS token-type) 'SUB)
                       (else token-type)))
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
  (test "a" '(ID "a" 1))
  (test "2" '(INT 2 1))
  (test "2 + 5" '(ADD (INT 2 1) (INT 5 1)))
  (test "-2" '(MINUS (INT 2 1)))
  (test "a * -2" '(MUL (ID "a" 1) (MINUS (INT 2 1))))
  (test "a * 2" '(MUL (ID "a" 1) (INT 2 1)))
  (test "-a * 2" '(MUL (MINUS (ID "a" 1)) (INT 2 1)))
  (test "b / -a * 2" '(DIV (ID "b" 1) (MUL (MINUS (ID "a" 1)) (INT 2 1))))
  (test "a + 2" '(ADD (ID "a" 1) (INT 2 1)))
  (test "3*a + 45" '(ADD (MUL (INT 3 1) (ID "a" 1)) (INT 45 1)))
  (test "a/5 - 99" '(SUB (DIV (ID "a" 1) (INT 5 1)) (INT 99 1)))
  (test "a + b + c" '(ADD (ID "a" 1) (ADD (ID "b" 1) (ID "c" 1))))
  (test "(a)" '(ID "a" 1))
  (test "(a+b)" '(ADD (ID "a" 1) (ID "b" 1)))
  (test "3*(a+b)" '(MUL (INT 3 1) (ADD (ID "a" 1) (ID "b" 1))))
  (test "(a+b)/2" '(DIV (ADD (ID "a" 1) (ID "b" 1)) (INT 2 1)))
  (test "(a + b) * (c - d)" '(MUL (ADD (ID "a" 1) (ID "b" 1)) (SUB (ID "c" 1) (ID "d" 1)))))

;; expr = term | term (EQ|NE|LT|LE|GT|GE) expr
(define (match-expr lexer token)
  (parse-trace "match-expr" token)
  (let ((t (match-term lexer token)))
    (if t
        (let* ((op-token (get-token lexer))
               (token-type (car op-token)))
          (cond
           ((or (eq? 'EQ token-type)
                (eq? 'NE token-type)
                (eq? 'LE token-type)
                (eq? 'LT token-type)
                (eq? 'GE token-type)
                (eq? 'GT token-type))
            (let ((e (match-expr lexer (get-token lexer))))
              (if e
                  (list token-type t e)
                  (parse-error "Missing expr after relation"))))
           (else
            (unget-token lexer op-token)
            t)))
        #f)))

(define (test-match-expr)
  (define test (make-tester match-expr))
  (test "a" '(ID "a" 1))
  (test "2" '(INT 2 1))
  (test "2 + 5" '(ADD (INT 2 1) (INT 5 1)))
  (test "-2" '(MINUS (INT 2 1)))
  (test "a * -2" '(MUL (ID "a" 1) (MINUS (INT 2 1))))
  (test "a * 2" '(MUL (ID "a" 1) (INT 2 1)))
  (test "-a * 2" '(MUL (MINUS (ID "a" 1)) (INT 2 1)))
  (test "b / -a * 2" '(DIV (ID "b" 1) (MUL (MINUS (ID "a" 1)) (INT 2 1))))
  (test "a + 2" '(ADD (ID "a" 1) (INT 2 1)))
  (test "3*a + 45" '(ADD (MUL (INT 3 1) (ID "a" 1)) (INT 45 1)))
  (test "a/5 - 99" '(SUB (DIV (ID "a" 1) (INT 5 1)) (INT 99 1)))
  (test "a + b + c" '(ADD (ID "a" 1) (ADD (ID "b" 1) (ID "c" 1))))
  (test "(a)" '(ID "a" 1))
  (test "(a+b)" '(ADD (ID "a" 1) (ID "b" 1)))
  (test "3*(a+b)" '(MUL (INT 3 1) (ADD (ID "a" 1) (ID "b" 1))))
  (test "(a+b)/2" '(DIV (ADD (ID "a" 1) (ID "b" 1)) (INT 2 1)))
  (test "(a + b) * (c - d)" '(MUL (ADD (ID "a" 1) (ID "b" 1)) (SUB (ID "c" 1) (ID "d" 1))))
  (test "a = b" '(EQ (ID "a" 1) (ID "b" 1)))
  (test "5 > 6" '(GT (INT 5 1) (INT 6 1)))
  (test "1+2 < 4*5" '(LT (ADD (INT 1 1) (INT 2 1)) (MUL (INT 4 1) (INT 5 1))))
  (test "(a > 6*d)" '(GT (ID "a" 1) (MUL (INT 6 1) (ID "d" 1))))
  (test "(c >= 99)" '(GE (ID "c" 1) (INT 99 1)))
  (test "a or b" '(OR (ID "a" 1) (ID "b" 1)))
  (test "c & d" '(AND (ID "c" 1) (ID "d" 1)))
  (test "(a > 6*d) or (c >= 99)" '(OR (GT (ID "a" 1) (MUL (INT 6 1) (ID "d" 1))) (GE (ID "c" 1) (INT 99 1))))
  (test "-(a+b)" '(MINUS (ADD (ID "a" 1) (ID "b" 1))))
  (test "a # b" '(NE (ID "a" 1) (ID "b" 1)))
  (test "(a = b) or (c # d)" '(OR (EQ (ID "a" 1) (ID "b" 1)) (NE (ID "c" 1) (ID "d" 1))))
  (test "~((a = b) or (c # d))" '(NOT (OR (EQ (ID "a" 1) (ID "b" 1)) (NE (ID "c" 1) (ID "d" 1)))))
  (test "~((a.xyzzy = b[99-z]) or (c[4].offset # d.last[52-p]))" '(NOT (OR (EQ (FIELD (ID "a" 1) (ID "xyzzy" 1)) (INDEX (ID "b" 1) (SUB (INT 99 1) (ID "z" 1)))) (NE (FIELD (INDEX (ID "c" 1) (INT 4 1)) (ID "offset" 1)) (INDEX (FIELD (ID "d" 1) (ID "last" 1)) (SUB (INT 52 1) (ID "p" 1))))))))

(define (test-all)
  (test-match-primary)
  (test-match-unary)
  (test-match-location)
  (test-match-factor)
  (test-match-term)
  (test-match-expr))
