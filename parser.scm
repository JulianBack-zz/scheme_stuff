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
;;; 4. DONE function calls
;;; 5. DONE statements: assignment, IF, WHILE, REPEAT, procedure call
;;; 6. DONE Strings in expressions
;;; 8. Add variable declarations
;;; 8. Add procedure declarations
;;; 9. Add type declarations
;;; 10. Add const declarations
;;; 11. Add Module statement

(import (chicken format))

; Return line number from a token which is a 2 or 3 item list
(define (line-number token)
  (if (eq? 2 (length token))
      (cadr token)
      (caddr token)))

(define (parse-error token msg)
  (printf "Error line ~A: ~A~N" (line-number token) msg)
  #f)

(define *parse-trace* #f)

;;; Number of tests executed
(define *test-count* 0)

;;; Number of tests failed
(define *test-failures* 0)

;;; Runs a sequence of tests
;;; name - name of function being tested
;;; match-function - the function
;;; tests list of tests.  Each test is a list of string to parse and expected parse tree
;;; TODO - get rid of global variables
(define (test-run name match-function tests)
  (let ((test (make-tester match-function)))
    (printf "Testing ~A~N" name)
    (set! *test-count* 0)
    (set! *test-failures* 0)
    (let loop ((t (car tests)) (rest (cdr tests)))
      (apply test t)
      (if (not (null? rest))
          (loop (car rest) (cdr rest))))
    (test-summary)))

; Returns a test function for a parse function
(define (make-tester match-function)
  (lambda (str expected)
    (define (parse lexer)
      (match-function lexer (get-token lexer)))
    (let ((result (parse (make-lexer-from-string str))))
      (if (equal? result expected)
          (test-success)
          (test-fail str result expected)))))

(define (test-summary)
  (if (positive? *test-failures*)
      (printf "~A tests / ~A errors~N" *test-count* *test-failures*)))

(define (test-success)
  (set! *test-count* (+ *test-count* 1)))

(define (test-fail str result expected)
  (set! *test-count* (+ *test-count* 1))
  (set! *test-failures* (+ *test-failures* 1))
  (display str)
  (display " FAIL GOT")
  (newline)
  (write result)
  (newline)
  (display "EXPECTED")
  (newline)
  (write expected)
  (newline))

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
          (parse-error t "Bad field"))))
   ((eq? 'OPENSQ (car token))
    (let ((e (match-expr lexer (get-token lexer))))
      (if (and e (eq? 'CLOSESQ (car (get-token lexer))))
          (list 'INDEX e)
          (parse-error token "Expected ]"))))
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
  (test-run "match-location" match-location
            '(("a" (ID "a" 1))
              ("a.b" (FIELD (ID "a" 1) (ID "b" 1)))
              ("a.b.c" (FIELD (FIELD (ID "a" 1) (ID "b" 1)) (ID "c" 1)))
              ("a[5]" (INDEX (ID "a" 1) (INT 5 1)))
              ("a[x+1]" (INDEX (ID "a" 1) (ADD (ID "x" 1) (INT 1 1))))
              ("a[x+1][y]" (INDEX (INDEX (ID "a" 1) (ADD (ID "x" 1) (INT 1 1))) (ID "y" 1)))
              ("a[x+1].b" (FIELD (INDEX (ID "a" 1) (ADD (ID "x" 1) (INT 1 1))) (ID "b" 1)))
              ("a.b[4].c" (FIELD (INDEX (FIELD (ID "a" 1) (ID "b" 1)) (INT 4 1)) (ID "c" 1))))))

;; actual-parameters = [expr] {COMMA expr}
(define (match-actual-parameters lexer token)
  (parse-trace "match-actual-parameters " token) 
  (if (eq? 'CLOSE (car token))
      (begin
        (unget-token lexer token)
        '())
      (let ((e (match-expr lexer token)))
        (if e
            (let loop ((t (get-token lexer)) (p (list e)))
              (cond
               ((eq? 'COMMA (car t))
                (let ((e (match-expr lexer (get-token lexer))))
                  (if e
                      (loop (get-token lexer) (cons e p))
                      #f)))
               (else
                (unget-token lexer t)
                (reverse p))))
            #f))))

(define (test-match-actual-parameters)
  (test-run "match-actual-parameters" match-actual-parameters
            '((")" ())
              ("a" ((ID "a" 1)))
              ("99" ((INT 99 1)))
              ("a, b" ((ID "a" 1) (ID "b" 1)))
              ("1+2, b*c" ((ADD (INT 1 1) (INT 2 1)) (MUL (ID "b" 1) (ID "c" 1))))
              ("1,2,3,4" ((INT 1 1) (INT 2 1) (INT 3 1) (INT 4 1))))))

;; primary = location | location OPEN actual-parameters CLOSE | INTEGER | STRING | OPEN expr CLOSE
(define (match-primary lexer token)
  (parse-trace "match-primary " token) 
  (let ((l (match-location lexer token)))
    (if l
        (let ((token (get-token lexer)))
          (cond
           ((eq? 'OPEN (car token))
            (let ((p (match-actual-parameters lexer (get-token lexer))))
              (if p
                  (let ((t (get-token lexer)))
                    (if (eq? 'CLOSE (car t))
                        (list 'CALL l p)
                        (parse-error t "Expected )")))
                  #f)))
           (else
            (unget-token lexer token)
            l)))
        (let ((token-type (car token)))
          (cond
           ((eq? 'INT token-type) token)
           ((eq? 'STRING token-type) token)
           ((eq? 'OPEN (car token))
            (let ((t (match-expr lexer (get-token lexer))))
              (if (and t (eq? 'CLOSE (car (get-token lexer))))
                  t
                  (parse-error token "Expected )"))))
           (else (parse-error token "Expected ID or INTEGER")))))))

(define (test-match-primary)
  (test-run "match-primary" match-primary
            '(("2" (INT 2 1))
              ("1232" (INT 1232 1))
              ("a" (ID "a" 1))
              ("xyzzy" (ID "xyzzy" 1))
              ("(a)" (ID "a" 1))
              ("(a+b)" (ADD (ID "a" 1) (ID "b" 1)))
              ("(a/b)" (DIV (ID "a" 1) (ID "b" 1)))
              ("(-a)" (MINUS (ID "a" 1)))
              ("test(a,b,c)" (CALL (ID "test" 1) ((ID "a" 1) (ID "b" 1) (ID "c" 1))))
              ("random()" (CALL (ID "random" 1) ()))
              ("sqr(2)" (CALL (ID "sqr" 1) ((INT 2 1)))))))

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
  (test-run "match-unary" match-unary
            '(("2" (INT 2 1))
              ("1232" (INT 1232 1))
              ("-45" (MINUS (INT 45 1)))
              ("a" (ID "a" 1))
              ("-b" (MINUS (ID "b" 1)))
              ("-(a+b)" (MINUS (ADD (ID "a" 1) (ID "b" 1)))))))

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
                  (parse-error token "Missing factor after * or /"))))
           (else
            (unget-token lexer op-token)
            u)))
        #f)))

(define (test-match-factor)
  (test-run "match-factor" match-factor
            '(("a" (ID "a" 1))
              ("2" (INT 2 1))
              ("-2" (MINUS (INT 2 1)))
              ("a * -2" (MUL (ID "a" 1) (MINUS (INT 2 1))))
              ("a * 2" (MUL (ID "a" 1) (INT 2 1)))
              ("-a * 2" (MUL (MINUS (ID "a" 1)) (INT 2 1)))
              ("b / -a * 2" (DIV (ID "b" 1) (MUL (MINUS (ID "a" 1)) (INT 2 1))))
              ("2*(b / -a * 2)" (MUL (INT 2 1) (DIV (ID "b" 1) (MUL (MINUS (ID "a" 1)) (INT 2 1)))))
              ("(b / -a * 2)/x" (DIV (DIV (ID "b" 1) (MUL (MINUS (ID "a" 1)) (INT 2 1))) (ID "x" 1))))))

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
                  (parse-error op-token "Missing term after + or -"))))
           (else
            (unget-token lexer op-token)
            f)))
        #f)))

(define (test-match-term)
  (test-run "match-term" match-term
            '(("a" (ID "a" 1))
              ("2" (INT 2 1))
              ("2 + 5" (ADD (INT 2 1) (INT 5 1)))
              ("-2" (MINUS (INT 2 1)))
              ("a * -2" (MUL (ID "a" 1) (MINUS (INT 2 1))))
              ("a * 2" (MUL (ID "a" 1) (INT 2 1)))
              ("-a * 2" (MUL (MINUS (ID "a" 1)) (INT 2 1)))
              ("b / -a * 2" (DIV (ID "b" 1) (MUL (MINUS (ID "a" 1)) (INT 2 1))))
              ("a + 2" (ADD (ID "a" 1) (INT 2 1)))
              ("3*a + 45" (ADD (MUL (INT 3 1) (ID "a" 1)) (INT 45 1)))
              ("a/5 - 99" (SUB (DIV (ID "a" 1) (INT 5 1)) (INT 99 1)))
              ("a + b + c" (ADD (ID "a" 1) (ADD (ID "b" 1) (ID "c" 1))))
              ("(a)" (ID "a" 1))
              ("(a+b)" (ADD (ID "a" 1) (ID "b" 1)))
              ("3*(a+b)" (MUL (INT 3 1) (ADD (ID "a" 1) (ID "b" 1))))
              ("(a+b)/2" (DIV (ADD (ID "a" 1) (ID "b" 1)) (INT 2 1)))
              ("(a + b) * (c - d)" (MUL (ADD (ID "a" 1) (ID "b" 1)) (SUB (ID "c" 1) (ID "d" 1)))))))

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
                  (parse-error op-token "Missing expr after relation"))))
           (else
            (unget-token lexer op-token)
            t)))
        #f)))

(define (test-match-expr)
  (test-run "match-expr" match-expr
            '(("a" (ID "a" 1))
              ("2" (INT 2 1))
              ("2 + 5" (ADD (INT 2 1) (INT 5 1)))
              ("-2" (MINUS (INT 2 1)))
              ("a * -2" (MUL (ID "a" 1) (MINUS (INT 2 1))))
              ("a * 2" (MUL (ID "a" 1) (INT 2 1)))
              ("-a * 2" (MUL (MINUS (ID "a" 1)) (INT 2 1)))
              ("b / -a * 2" (DIV (ID "b" 1) (MUL (MINUS (ID "a" 1)) (INT 2 1))))
              ("a + 2" (ADD (ID "a" 1) (INT 2 1)))
              ("3*a + 45" (ADD (MUL (INT 3 1) (ID "a" 1)) (INT 45 1)))
              ("a/5 - 99" (SUB (DIV (ID "a" 1) (INT 5 1)) (INT 99 1)))
              ("a + b + c" (ADD (ID "a" 1) (ADD (ID "b" 1) (ID "c" 1))))
              ("(a)" (ID "a" 1))
              ("(a+b)" (ADD (ID "a" 1) (ID "b" 1)))
              ("3*(a+b)" (MUL (INT 3 1) (ADD (ID "a" 1) (ID "b" 1))))
              ("(a+b)/2" (DIV (ADD (ID "a" 1) (ID "b" 1)) (INT 2 1)))
              ("(a + b) * (c - d)" (MUL (ADD (ID "a" 1) (ID "b" 1)) (SUB (ID "c" 1) (ID "d" 1))))
              ("a = b" (EQ (ID "a" 1) (ID "b" 1)))
              ("5 > 6" (GT (INT 5 1) (INT 6 1)))
              ("1+2 < 4*5" (LT (ADD (INT 1 1) (INT 2 1)) (MUL (INT 4 1) (INT 5 1))))
              ("(a > 6*d)" (GT (ID "a" 1) (MUL (INT 6 1) (ID "d" 1))))
              ("(c >= 99)" (GE (ID "c" 1) (INT 99 1)))
              ("a or b" (OR (ID "a" 1) (ID "b" 1)))
              ("c & d" (AND (ID "c" 1) (ID "d" 1)))
              ("(a > 6*d) or (c >= 99)" (OR (GT (ID "a" 1) (MUL (INT 6 1) (ID "d" 1))) (GE (ID "c" 1) (INT 99 1))))
              ("-(a+b)" (MINUS (ADD (ID "a" 1) (ID "b" 1))))
              ("a # b" (NE (ID "a" 1) (ID "b" 1)))
              ("(a = b) or (c # d)" (OR (EQ (ID "a" 1) (ID "b" 1)) (NE (ID "c" 1) (ID "d" 1))))
              ("~((a = b) or (c # d))" (NOT (OR (EQ (ID "a" 1) (ID "b" 1)) (NE (ID "c" 1) (ID "d" 1)))))
              ("~((a.xyzzy = b[99-z]) or (c[4].offset # d.last[52-p]))" (NOT (OR (EQ (FIELD (ID "a" 1) (ID "xyzzy" 1)) (INDEX (ID "b" 1) (SUB (INT 99 1) (ID "z" 1)))) (NE (FIELD (INDEX (ID "c" 1) (INT 4 1)) (ID "offset" 1)) (INDEX (FIELD (ID "d" 1) (ID "last" 1)) (SUB (INT 52 1) (ID "p" 1)))))))
              ("plot(sin(x*2)+6, pi*cos(y/4)+8)" (CALL (ID "plot" 1) ((ADD (CALL (ID "sin" 1) ((MUL (ID "x" 1) (INT 2 1)))) (INT 6 1)) (ADD (MUL (ID "pi" 1) (CALL (ID "cos" 1) ((DIV (ID "y" 1) (INT 4 1))))) (INT 8 1)))))
              ("0ABCH" (INT 2748 1))
              ("a div b" (DIV (ID "a" 1) (ID "b" 1))))))

;;; Match optional else clause.  Return '() if no else clause, #f if error
(define (match-else lexer token)
  (parse-trace "match-else" token)
  (if (eq? 'ELSE (car token))
      (let ((s (match-statement-list lexer (get-token lexer))))
        (if s
            (list 'ELSE s)
            (parse-error "Bad statements after ELSE")))
      (begin
        (unget-token lexer token)
        '())))

;;; Match optional elseif clauses.  Return '() if no elseif clauses, #f if error
(define (match-elseif lexer token)
  (parse-trace "match-elseif" token)
  (let loop ((t token) (eif '()))
    ;;(display "loop ") (write t) (newline)
    (if (eq? 'ELSEIF (car t))
        (let ((e (match-expr lexer (get-token lexer))))
          (if e
              (let ((t (get-token lexer)))
                (if (eq? 'THEN (car t))
                    (let ((s (match-statement-list lexer (get-token lexer))))
                      (if s
                          (loop (get-token lexer) (cons (cons e s) eif))
                          #f))
                    (parse-error t "Expected THEN after ELSEIF")))
              #f))
        (begin
          (unget-token lexer t)
          (reverse eif)))))

;;; if-statement = IF expr THEN statement-list {ELSEIF expr THEN statement-list} [ELSE statement-list] END
(define (match-if-statement lexer token)
  (parse-trace "match-if-statement" token)
  (if (eq? 'IF (car token))
      (let ((e (match-expr lexer (get-token lexer))))
        (if e
            (let ((t (get-token lexer)))
              (if (eq? 'THEN (car t))
                  (let ((s (match-statement-list lexer (get-token lexer))))
                    (if s
                        (let ((elseif (match-elseif lexer (get-token lexer))))
                          (if elseif
                              (let ((els (match-else lexer (get-token lexer))))
                                (if els
                                    (let ((t (get-token lexer)))
                                      (if (eq? 'END (car t))
                                          (if (null? elseif)
                                              (if (null? els)
                                                  (list 'COND (cons e s))
                                                  (list 'COND (cons e s) els))
                                              (if (null? els)
                                                  (append (list 'COND (cons e s) elseif))
                                                  (append (list 'COND (cons e s) elseif (list els)))))
                                          (parse-error t "Expected END in IF")))
                                    #f))
                              #f))
                        #f))
                  (parse-error t "Expected THEN")))
            (parse-error token "Bad expression in IF")))
      #f))

(define (test-match-if-statement)
  (test-run "match-if-statement" match-if-statement
            '(
              ("if a = b then x := y end"
               (COND ((EQ (ID "a" 1) (ID "b" 1))
                      (ASSIGN (ID "x" 1) (ID "y" 1)))))
              ("if a + 1 >= fred() then a := b; c:= d else a := a + 1 end"
               (COND ((GE (ADD (ID "a" 1) (INT 1 1)) (CALL (ID "fred" 1) ()))
                      (ASSIGN (ID "a" 1) (ID "b" 1))
                      (ASSIGN (ID "c" 1) (ID "d" 1)))
                     (ELSE
                      ((ASSIGN (ID "a" 1) (ADD (ID "a" 1) (INT 1 1)))))))
              ("if a > 99 then b := 54 end"
               (COND ((GT (ID "a" 1) (INT 99 1))
                      (ASSIGN (ID "b" 1) (INT 54 1)))))
              ("if a > 99 then b := 54 elseif a > 45 then b := 21 end"
               (COND ((GT (ID "a" 1) (INT 99 1))
                      (ASSIGN (ID "b" 1) (INT 54 1)))
                     (((GT (ID "a" 1) (INT 45 1))
                       (ASSIGN (ID "b" 1) (INT 21 1))))))
              ("if a > 99 then b := 54 elseif a > 45 then b := 21 elseif a > 13 then b := 11 end"
               (COND ((GT (ID "a" 1) (INT 99 1))
                      (ASSIGN (ID "b" 1) (INT 54 1)))
                     (((GT (ID "a" 1) (INT 45 1))
                       (ASSIGN (ID "b" 1) (INT 21 1)))
                      ((GT (ID "a" 1) (INT 13 1))
                       (ASSIGN (ID "b" 1) (INT 11 1))))))
              ("if a > 99 then b := 54 elseif a > 45 then b := 21 elseif a > 13 then b := 11 else b := 3 end"
               (COND ((GT (ID "a" 1) (INT 99 1))
                      (ASSIGN (ID "b" 1) (INT 54 1)))
                     (((GT (ID "a" 1) (INT 45 1))
                       (ASSIGN (ID "b" 1) (INT 21 1)))
                      ((GT (ID "a" 1) (INT 13 1))
                       (ASSIGN (ID "b" 1) (INT 11 1))))
                     ((ELSE ((ASSIGN (ID "b" 1) (INT 3 1))))))))))

;;; while-statement = WHILE expr DO statements END
(define (match-while-do lexer token)
  (parse-trace "match-while-do" token)
  (if (eq? 'WHILE (car token))
      (let ((e (match-expr lexer (get-token lexer))))
        (if e
            (let ((t (get-token lexer)))
              (if (eq? 'DO (car t))
                  (let ((s (match-statement-list lexer (get-token lexer))))
                    (if s
                        (let ((t (get-token lexer)))
                          (if (eq? 'END (car t))
                              (list 'WHILE e s)
                              (parse-error t "Expected END in WHILE")))
                        #f))
                  #f))
            (parse-error token "Expected DO")))
      #f))

(define (test-match-while-do)
  (test-run "match-while-do" match-while-do
            '(
              ("while a do b() end"
               (WHILE (ID "a" 1)
                      ((CALL (ID "b" 1) ()))))
              ("while a < b do a := a + 1; print(a) end"
               (WHILE (LT (ID "a" 1) (ID "b" 1))
                      ((ASSIGN (ID "a" 1) (ADD (ID "a" 1) (INT 1 1)))
                       (CALL (ID "print" 1) ((ID "a" 1)))))))))

;;; repeat-statement = REPEAT statements UNTIL expr
(define (match-repeat-until lexer token)
  (parse-trace "match-repeat-until" token)
  (if (eq? 'REPEAT (car token))
      (let ((s (match-statement-list lexer (get-token lexer))))
        (if s
            (let ((t (get-token lexer)))
              (if (eq? 'UNTIL (car t))
                  (let ((e (match-expr lexer (get-token lexer))))
                    (if e
                        (list 'REPEAT s e)
                        #f))
                  (parse-error t "Expected UNTIL")))
            #f))
      #f))

(define (test-match-repeat-until)
  (test-run "match-repeat-until" match-repeat-until
            '(
              ("repeat b() until a"
               (REPEAT ((CALL (ID "b" 1) ()))
                       (ID "a" 1)))
              ("repeat a := a + 1; print(a) until a >= b"
               (REPEAT ((ASSIGN (ID "a" 1) (ADD (ID "a" 1) (INT 1 1)))
                        (CALL (ID "print" 1) ((ID "a" 1))))
                       (GE (ID "a" 1) (ID "b" 1)))))))

;;; assigment = location ASSIGN expr
;;; procedure_call = location OPEN actual-parameters CLOSE
(define (match-assignment-or-procedure-call lexer token)
  (parse-trace "match-assignment-or-procedure-call" token)
  (let ((l (match-location lexer token)))
    (if l
        (let ((t (get-token lexer)))
          (cond
           ((eq? 'ASSIGN (car t))
            (let ((e (match-expr lexer (get-token lexer))))
              (if e
                  (list 'ASSIGN l e)
                  #f)))
           ((eq? 'OPEN (car t))
            (let ((p (match-actual-parameters lexer (get-token lexer))))
              (if p
                  (let ((t (get-token lexer)))
                    (if (eq? 'CLOSE (car t))
                        (list 'CALL l p)
                        (parse-error t "Expected )")))
                  #f)))
           (else
            (parse-error t "Expected := or ("))))
        #f)))

(define (test-match-assignment-or-procedure-call)
  (test-run "match-assignment-or-procedure-call" match-assignment-or-procedure-call
            '(("a := b" (ASSIGN (ID "a" 1) (ID "b" 1)))
              ("a := b + c" (ASSIGN (ID "a" 1) (ADD (ID "b" 1) (ID "c" 1))))
              ("a.x := b / c" (ASSIGN (FIELD (ID "a" 1) (ID "x" 1)) (DIV (ID "b" 1) (ID "c" 1))))
              ("a.x[99] := 2+4*b.c" (ASSIGN (INDEX (FIELD (ID "a" 1) (ID "x" 1)) (INT 99 1)) (ADD (INT 2 1) (MUL (INT 4 1) (FIELD (ID "b" 1) (ID "c" 1))))))
              ("x[99] := 2 + 3 / 5" (ASSIGN (INDEX (ID "x" 1) (INT 99 1)) (ADD (INT 2 1) (DIV (INT 3 1) (INT 5 1)))))
              ("z[55].g := func(1,2,3)" (ASSIGN (FIELD (INDEX (ID "z" 1) (INT 55 1)) (ID "g" 1)) (CALL (ID "func" 1) ((INT 1 1) (INT 2 1) (INT 3 1)))))
              ("func(1,2,3)" (CALL (ID "func" 1) ((INT 1 1) (INT 2 1) (INT 3 1))))
              ("func(6+x,sin(y))" (CALL (ID "func" 1) ((ADD (INT 6 1) (ID "x" 1)) (CALL (ID "sin" 1) ((ID "y" 1))))))
              ("x.y(1,3,5)" (CALL (FIELD (ID "x" 1) (ID "y" 1)) ((INT 1 1) (INT 3 1) (INT 5 1))))
              ("y[55](6,7,8)" (CALL (INDEX (ID "y" 1) (INT 55 1)) ((INT 6 1) (INT 7 1) (INT 8 1)))))))

;;; statement = if-statement | while-statement | repeat-statement | assignment | procedure_call | <blank>
(define (match-statement lexer token)
  (parse-trace "match-statement" token)
  (let ((ap (match-assignment-or-procedure-call lexer token)))
    (if ap
        ap
        (let ((ifs (match-if-statement lexer token)))
          (if ifs
              ifs
              (let ((wh (match-while-do lexer token)))
                (if wh
                    wh
                    (let ((rp (match-repeat-until lexer token)))
                      (if rp
                          rp
                          #f)))))))))

(define (test-match-statement)
  (test-run "match-statement" match-statement
            '(("a := b" (ASSIGN (ID "a" 1) (ID "b" 1)))
              ("a := b + c" (ASSIGN (ID "a" 1) (ADD (ID "b" 1) (ID "c" 1))))
              ("a.x := b / c" (ASSIGN (FIELD (ID "a" 1) (ID "x" 1)) (DIV (ID "b" 1) (ID "c" 1))))
              ("a.x[99] := 2+4*b.c" (ASSIGN (INDEX (FIELD (ID "a" 1) (ID "x" 1)) (INT 99 1)) (ADD (INT 2 1) (MUL (INT 4 1) (FIELD (ID "b" 1) (ID "c" 1))))))
              ("x[99] := 2 + 3 / 5" (ASSIGN (INDEX (ID "x" 1) (INT 99 1)) (ADD (INT 2 1) (DIV (INT 3 1) (INT 5 1)))))
              ("z[55].g := func(1,2,3)" (ASSIGN (FIELD (INDEX (ID "z" 1) (INT 55 1)) (ID "g" 1)) (CALL (ID "func" 1) ((INT 1 1) (INT 2 1) (INT 3 1)))))
              ("func(1,2,3)" (CALL (ID "func" 1) ((INT 1 1) (INT 2 1) (INT 3 1))))
              ("func(6+x,sin(y))" (CALL (ID "func" 1) ((ADD (INT 6 1) (ID "x" 1)) (CALL (ID "sin" 1) ((ID "y" 1))))))
              ("x.y(1,3,5)" (CALL (FIELD (ID "x" 1) (ID "y" 1)) ((INT 1 1) (INT 3 1) (INT 5 1))))
              ("y[55](6,7,8)" (CALL (INDEX (ID "y" 1) (INT 55 1)) ((INT 6 1) (INT 7 1) (INT 8 1)))))))


;;; statement-list = [statement] {; statement}
(define (match-statement-list lexer token)
  (parse-trace "match-statement-list" token)
  (let ((s (match-statement lexer token)))
    (if s
        (let loop ((stats (list s)) (token (get-token lexer)))
          (if (eq? 'SEMICOLON (car token))
              (let ((s (match-statement lexer (get-token lexer))))
                (if s
                    (loop (cons s stats) (get-token lexer))
                    (parse-error token "Expected statement")))
              (begin
                (unget-token lexer token)
                (reverse stats))))
        '())))

(define (test-match-statement-list)
  (test-run "match-statement-list" match-statement-list
            '(("a := b" ((ASSIGN (ID "a" 1) (ID "b" 1))))
              ("a := b; d := c" ((ASSIGN (ID "a" 1) (ID "b" 1)) (ASSIGN (ID "d" 1) (ID "c" 1))))
              ("func(1,2,3)" ((CALL (ID "func" 1) ((INT 1 1) (INT 2 1) (INT 3 1)))))
              ("func(1,2,3); x := y +1" ((CALL (ID "func" 1) ((INT 1 1) (INT 2 1) (INT 3 1))) (ASSIGN (ID "x" 1) (ADD (ID "y" 1) (INT 1 1)))))
              ("" ())
              (";;" ())
              ("x := 1;\ny := 2;\nwhile x > y do\n  if x > 2* y then\n    print(\"wow\")\n  else\n    print(\"ok\")\n  end\nend"
               ((ASSIGN (ID "x" 1) (INT 1 1))
                (ASSIGN (ID "y" 2) (INT 2 2))
                (WHILE (GT (ID "x" 3) (ID "y" 3))
                       ((COND ((GT (ID "x" 4) (MUL (INT 2 4) (ID "y" 4)))
                               (CALL (ID "print" 5) ((STRING "wow" 5))))
                              (ELSE
                               ((CALL (ID "print" 7) ((STRING "ok" 7))))))))))
              ("repeat\n  x := x + 1;\n  y := x*2;\n  if y - x > 6 then\n    print(\"big stuff\")\n  end;\n  p := p+3\nuntil p > 99;\nprint(x, y)"
               ((REPEAT
                 ((ASSIGN (ID "x" 2) (ADD (ID "x" 2) (INT 1 2)))
                  (ASSIGN (ID "y" 3) (MUL (ID "x" 3) (INT 2 3)))
                  (COND ((GT (SUB (ID "y" 4) (ID "x" 4)) (INT 6 4))
                         (CALL (ID "print" 5) ((STRING "big stuff" 5)))))
                  (ASSIGN (ID "p" 7) (ADD (ID "p" 7) (INT 3 7))))
                 (GT (ID "p" 8) (INT 99 8)))
                (CALL (ID "print" 9) ((ID "x" 9) (ID "y" 9))))))))

;;; ident-list = ID {"," ID}.
(define (match-id-list lexer token)
  (parse-trace "match-id-list" token)
  (if (eq? 'ID (car token))
      (let loop ((t (get-token lexer)) (id-list (list token)))
        (if (eq? 'COMMA (car t))
            (let ((id (get-token lexer)))
              (if (eq? 'ID (car id))
                  (loop (get-token lexer) (cons id id-list))
                  (parse-error id "Expected ID")))
            (begin
              (unget-token lexer t)
              (reverse id-list))))
      (parse-error token "Expected ID")))

(define (test-match-id-list)
  (test-run "match-id-list" match-id-list
            '(("hello" ((ID "hello" 1)))
              ("hello, world" ((ID "hello" 1) (ID "world" 1)))
              ("a,b,c,d" ((ID "a" 1) (ID "b" 1) (ID "c" 1) (ID "d" 1))))))

;;; type = ID | array-type | record-type.
(define (match-type lexer token)
  (parse-trace "match-type" token)
  (if (eq? 'ID (car token))
      (cons 'TYPE (cdr token))
      (let ((array (match-array-type lexer token)))
        (if array
            array
            ;; TODO - add records
            (parse-error token "Bad type")))))

(define (test-match-type)
  (test-run "match-type" match-type
            '(("integer" (TYPE "integer" 1))
              ("string" (TYPE "string" 1))
              ("array 10 of integer" (ARRAY (TYPE "integer" 1) (INT 10 1)))
              ("array 10 of array 4 of integer" (ARRAY (ARRAY (TYPE "integer" 1) (INT 4 1)) (INT 10 1))))))

;;; array-type = ARRAY expr OF type.
(define (match-array-type lexer token)
  (parse-trace "match-array-type" token)
  (if (eq? 'ARRAY (car token))
      (let ((e (match-expr lexer (get-token lexer))))
        (if e
            (let ((t (get-token lexer)))
              (if (eq? 'OF (car t))
                  (let ((type (match-type lexer (get-token lexer))))
                    (if type
                        (list 'ARRAY type e)
                        #f))
                  (parse-error t "Expected OF")))
            #f))
      #f))

;;; field-list = [ident-list COLON type].
;;; record-type = RECORD field-list {SEMICOLON field-list} END.
;;; var-declaration = VAR {ident-list: type SEMICOLON}
;;; type-declaration = [TYPE {ID EQ type SEMICOLON}]

;;; TODO
;;; procedure-declaration
;;; module

(define (test-all)
  (test-match-primary)
  (test-match-unary)
  (test-match-location)
  (test-match-factor)
  (test-match-term)
  (test-match-actual-parameters)
  (test-match-expr)
  (test-match-assignment-or-procedure-call)
  (test-match-statement)
  (test-match-statement-list)
  (test-match-if-statement)
  (test-match-while-do)
  (test-match-repeat-until)
  (test-match-id-list)
  (test-match-type))
