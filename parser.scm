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
;;; 8. DONE Add variable declarations
;;; 8. DONE Add procedure declarations
;;; 9. DONE Add type declarations
;;; 10. DONE Add const declarations
;;; 11. DONE Add Module statement
;;; 12. Refactor?  Use macros to shorten code?  Put tests in separate file?  Partially done.
;;; 13. Syntax doesn't allow for procedure return values.
;;; 14. Floating point?
;;; 15. DONE Parse from a file for longer tests.
;;; 16. IMPORT and EXPORT statements
;;; 17. Public markers

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

;;; Get the type of a token
(define-syntax get-token-type
  (syntax-rules ()
    ((get-token-type token) (car token))))

;;; Match an ID against a token
(define-syntax match-type?
  (syntax-rules ()
    ((match-type? id token-type) (eq? (quote id) token-type))))

;;; Match an ID against a token
(define-syntax match-token?
  (syntax-rules ()
    ((match-token? id token) (eq? (quote id) (car token)))))

;; desig = . ID | [ expr ]
(define (match-desig lexer token)
  (parse-trace "match-desig" token)
  (cond
   ((match-token? DOT token)
    (let ((t (get-token lexer)))
      (if (match-token? ID t)
          (list 'FIELD t)
          (parse-error t "Bad field"))))
   ((match-token? OPENSQ token)
    (let ((e (match-expr lexer (get-token lexer))))
      (if (and e (match-token? CLOSESQ (get-token lexer)))
          (list 'INDEX e)
          (parse-error token "Expected ]"))))
   (else 
    (unget-token lexer token)
    #f)))

;; location = ID {desig}
(define (match-location lexer token)
  (parse-trace "match-location" token)
  (if (match-token? ID token)
      (let loop ((d (match-desig lexer (get-token lexer))) (des token))
        (cond
         ((not d)
          des)
         ((match-token? FIELD d)
          (loop (match-desig lexer (get-token lexer)) (cons (car d) (cons des (cdr d)))))
         ((match-token? INDEX d)
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
  (if (match-token? CLOSE token)
      (begin
        (unget-token lexer token)
        '())
      (let ((e (match-expr lexer token)))
        (if e
            (let loop ((t (get-token lexer)) (p (list e)))
              (cond
               ((match-token? COMMA t)
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
           ((match-token? OPEN token)
            (let ((p (match-actual-parameters lexer (get-token lexer))))
              (if p
                  (let ((t (get-token lexer)))
                    (if (match-token? CLOSE t)
                        (list 'CALL l p)
                        (parse-error t "Expected )")))
                  #f)))
           (else
            (unget-token lexer token)
            l)))
        (cond
         ((match-token? INT token) token)
         ((match-token? STRING token) token)
         ((match-token? OPEN token)
            (let ((t (match-expr lexer (get-token lexer))))
              (if (and t (match-token? CLOSE (get-token lexer)))
                  t
                  (parse-error token "Expected )"))))
         (else (parse-error token "Expected ID or INTEGER"))))))

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
  (cond
   ((or (match-token? MINUS token)
        (match-token? NOT token))
    (let ((p (match-primary lexer (get-token lexer))))
      (if p (list (car token) p)
          #f)))
   (else (match-primary lexer token))))

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
        (let ((op-token (get-token lexer)))
          (cond
           ((or (match-token? MUL op-token)
                (match-token? DIV op-token)
                (match-token? MOD op-token)
                (match-token? AND op-token))
            (let ((f (match-factor lexer (get-token lexer))))
              (if f
                  (list (car op-token) u f)
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
               (token-type (get-token-type op-token)))
          (cond
           ((or (match-type? PLUS token-type)
                (match-type? MINUS token-type)
                (match-type? OR token-type))
            (let ((op (cond
                       ((match-type? PLUS token-type) 'ADD)
                       ((match-type? MINUS token-type) 'SUB)
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
               (token-type (get-token-type op-token)))
          (cond
           ((or (match-type? EQ token-type)
                (match-type? NE token-type)
                (match-type? LE token-type)
                (match-type? LT token-type)
                (match-type? GE token-type)
                (match-type? GT token-type))
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
  (if (match-token? ELSE token)
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
    (if (match-token? ELSEIF t)
        (let ((e (match-expr lexer (get-token lexer))))
          (if e
              (let ((t (get-token lexer)))
                (if (match-token? THEN t)
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
  (if (match-token? IF token)
      (let ((e (match-expr lexer (get-token lexer))))
        (if e
            (let ((t (get-token lexer)))
              (if (match-token? THEN t)
                  (let ((s (match-statement-list lexer (get-token lexer))))
                    (if s
                        (let ((elseif (match-elseif lexer (get-token lexer))))
                          (if elseif
                              (let ((els (match-else lexer (get-token lexer))))
                                (if els
                                    (let ((t (get-token lexer)))
                                      (if (match-token? END t)
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
  (if (match-token? WHILE token)
      (let ((e (match-expr lexer (get-token lexer))))
        (if e
            (let ((t (get-token lexer)))
              (if (match-token? DO t)
                  (let ((s (match-statement-list lexer (get-token lexer))))
                    (if s
                        (let ((t (get-token lexer)))
                          (if (match-token? END t)
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
  (if (match-token? REPEAT token)
      (let ((s (match-statement-list lexer (get-token lexer))))
        (if s
            (let ((t (get-token lexer)))
              (if (match-token? UNTIL t)
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
           ((match-token? ASSIGN t)
            (let ((e (match-expr lexer (get-token lexer))))
              (if e
                  (list 'ASSIGN l e)
                  #f)))
           ((match-token? OPEN t)
            (let ((p (match-actual-parameters lexer (get-token lexer))))
              (if p
                  (let ((t (get-token lexer)))
                    (if (match-token? CLOSE t)
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
          (if (match-token? SEMICOLON token)
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

;;; id-list = ID {"," ID}.
(define (match-id-list lexer token)
  (parse-trace "match-id-list" token)
  (if (match-token? ID token)
      (let loop ((t (get-token lexer)) (id-list (list token)))
        (if (match-token? COMMA t)
            (let ((id (get-token lexer)))
              (if (match-token? ID id)
                  (loop (get-token lexer) (cons id id-list))
                  (parse-error id "Expected ID")))
            (begin
              (unget-token lexer t)
              (reverse id-list))))
      #f))

(define (test-match-id-list)
  (test-run "match-id-list" match-id-list
            '(("hello" ((ID "hello" 1)))
              ("hello, world" ((ID "hello" 1) (ID "world" 1)))
              ("a,b,c,d" ((ID "a" 1) (ID "b" 1) (ID "c" 1) (ID "d" 1))))))

;;; type = ID | array-type | record-type.
(define (match-type lexer token)
  (parse-trace "match-type" token)
  (if (match-token? ID token)
      (cons 'TYPE (cdr token))
      (let ((array (match-array-type lexer token)))
        (if array
            array
            (let ((record (match-record-type lexer token)))
              (if record
                  record
                  (parse-error token "Bad type")))))))

(define (test-match-type)
  (test-run "match-type" match-type
            '(("integer" (TYPE "integer" 1))
              ("string" (TYPE "string" 1))
              ("array 10 of integer" (ARRAY (TYPE "integer" 1) (INT 10 1)))
              ("array 10 of array 4 of integer" (ARRAY (ARRAY (TYPE "integer" 1) (INT 4 1)) (INT 10 1))))))

;;; array-type = ARRAY expr OF type.
(define (match-array-type lexer token)
  (parse-trace "match-array-type" token)
  (if (match-token? ARRAY token)
      (let ((e (match-expr lexer (get-token lexer))))
        (if e
            (let ((t (get-token lexer)))
              (if (match-token? OF t)
                  (let ((type (match-type lexer (get-token lexer))))
                    (if type
                        (list 'ARRAY type e)
                        #f))
                  (parse-error t "Expected OF")))
            #f))
      #f))

;;; field-list = [id-list COLON type].
(define (match-field-list lexer token)
  (parse-trace "match-field-list" token)
  (let ((ids (match-id-list lexer token)))
    (if ids
        (let ((t (get-token lexer)))
          (if (match-token? COLON t)
              (let ((type (match-type lexer (get-token lexer))))
                (if type
                    (list 'FIELDS type ids)
                    #f))
              (parse-error t "Expected :")))
        (parse-error token "Expected ID"))))

(define (test-match-field-list)
  (test-run "match-field-list" match-field-list
            '(("a: integer" (FIELDS (TYPE "integer" 1) ((ID "a" 1))))
              ("a,b: integer" (FIELDS (TYPE "integer" 1) ((ID "a" 1) (ID "b" 1))))
              ("a,b,c: array 10 of integer" (FIELDS (ARRAY (TYPE "integer" 1) (INT 10 1)) ((ID "a" 1) (ID "b" 1) (ID "c" 1)))))))
                    
;;; record-type = RECORD field-list {SEMICOLON field-list} END.
(define (match-record-type lexer token)
  (parse-trace "match-record-type" token)
  (if (match-token? RECORD token)
      (let ((fields (match-field-list lexer (get-token lexer))))
        (if fields
            (let loop ((t (get-token lexer)) (flist (list fields)))
              (if (match-token? SEMICOLON t)
                  (let ((f (match-field-list lexer (get-token lexer))))
                    (if f
                        (loop (get-token lexer) (cons f flist))
                        #f))
                  (if (match-token? END t)
                      (list 'RECORD (reverse flist))
                      (parse-error t "expected END"))))
            #f))
      #f))

(define (test-match-record-type)
  (test-run "match-record-type" match-record-type
            '(("record\n  a: integer;\n  b: string\nend"
               (RECORD
                ((FIELDS (TYPE "integer" 2) ((ID "a" 2)))
                 (FIELDS (TYPE "string" 3) ((ID "b" 3))))))
              ("record\n  a,b: integer;\n  c: array 10 of char\nend"
               (RECORD
                ((FIELDS (TYPE "integer" 2) ((ID "a" 2) (ID "b" 2)))
                 (FIELDS (ARRAY (TYPE "char" 3) (INT 10 3)) ((ID "c" 3))))))
              ("record\n  a: record\n    b: integer;\n    c: char\n  end;\n  d: integer;\n  e: string\nend"
               (RECORD
                ((FIELDS (RECORD
                          ((FIELDS (TYPE "integer" 3) ((ID "b" 3))) (FIELDS (TYPE "char" 4) ((ID "c" 4)))))
                         ((ID "a" 2)))
                 (FIELDS (TYPE "integer" 6) ((ID "d" 6))) (FIELDS (TYPE "string" 7) ((ID "e" 7)))))))))

;;; var-declaration = VAR {ident-list COLON type SEMICOLON}
(define (match-var-declaration lexer token)
  (parse-trace "match-var-declaration" token)
  (if (match-token? VAR token)
      (let loop ((idt (get-token lexer)) (vars '()))
        ;(display "var loop ") (write idt) (newline) (write vars) (newline)
        (let ((ids (match-id-list lexer idt)))
          (if ids
              (let ((t (get-token lexer)))
                (if (match-token? COLON t)
                    (let ((type (match-type lexer (get-token lexer))))
                      (if type
                          (let ((ts (get-token lexer)))
                            (if (match-token? SEMICOLON ts)
                                (loop (get-token lexer) (cons (list type ids) vars))
                                (parse-error ts "Expected ;")))
                          #f))
                    (parse-error t "Expected :")))
              (begin
                (unget-token lexer idt)
                (cons 'VAR (reverse vars))))))
      #f))

(define (test-match-var-declaration)
 (test-run "match-var-declaration" match-var-declaration
           '(("var x: integer;"
              (VAR ((TYPE "integer" 1) ((ID "x" 1)))))
             ("var x,y: integer;"
              (VAR ((TYPE "integer" 1) ((ID "x" 1) (ID "y" 1)))))
             ("var x: integer;\n  y: string;"
              (VAR ((TYPE "integer" 1) ((ID "x" 1)))
                   ((TYPE "string" 2) ((ID "y" 2)))))
             ("var z: array 10 of integer;\n  a,b: integer;"
              (VAR ((ARRAY (TYPE "integer" 1) (INT 10 1)) ((ID "z" 1)))
                   ((TYPE "integer" 2) ((ID "a" 2) (ID "b" 2)))))
             ("var a: record\n  x:integer;\n  y: array 9 of char\nend;\nb: string;"
              (VAR ((RECORD ((FIELDS (TYPE "integer" 2) ((ID "x" 2)))
                             (FIELDS (ARRAY (TYPE "char" 3) (INT 9 3)) ((ID "y" 3))))) ((ID "a" 1)))
                   ((TYPE "string" 5) ((ID "b" 5))))))))

;;; type-declaration = [TYPE {ID EQ type SEMICOLON}]
(define (match-type-declaration lexer token)
  (parse-trace "match-type-declaration" token)
  (if (match-token? TYPE token)
      (let loop ((id (get-token lexer)) (tlist '()))
        (if (match-token? ID id)
            (let ((eq (get-token lexer)))
              (if (match-token? EQ eq)
                  (let ((type (match-type lexer (get-token lexer))))
                    (if type
                        (let ((sc (get-token lexer)))
                          (if (match-token? SEMICOLON sc)
                              (loop (get-token lexer) (cons (list id type) tlist))
                              (parse-error sc "Expected ;")))
                        #f))
                  (parse-error eq "Expected =")))
            (begin
              (unget-token lexer id)
              (cons 'TYPEDEF (reverse tlist)))))
      #f))

(define (test-match-type-declaration)
  (test-run "match-type-declaration" match-type-declaration
            '(("type name = array 10 of char;"
               (TYPEDEF ((ID "name" 1) (ARRAY (TYPE "char" 1) (INT 10 1)))))
              ("type point = record x, y: integer end;"
               (TYPEDEF ((ID "point" 1) (RECORD ((FIELDS (TYPE "integer" 1) ((ID "x" 1) (ID "y" 1))))))))
              ("type point = record x, y: integer end;\n  name = array 32 of char;"
               (TYPEDEF ((ID "point" 1) (RECORD ((FIELDS (TYPE "integer" 1) ((ID "x" 1) (ID "y" 1))))))
                        ((ID "name" 2) (ARRAY (TYPE "char" 2) (INT 32 2)))))
              ("type myint = integer;"
               (TYPEDEF ((ID "myint" 1) (TYPE "integer" 1)))))))

;;; const-declaration = CONST {ID EQ expr SEMICOLON}
(define (match-const-declaration lexer token)
  (parse-trace "match-const-declaration" token)
  (if (match-token? CONST token)
      (let loop ((id (get-token lexer)) (clist '()))
        (if (match-token? ID id)
            (let ((eq (get-token lexer)))
              (if (match-token? EQ eq)
                  (let ((expr (match-expr lexer (get-token lexer))))
                    (if expr
                        (let ((sc (get-token lexer)))
                          (if (match-token? SEMICOLON sc)
                              (loop (get-token lexer) (cons (list id expr) clist))
                              (parse-error sc "Expected ;")))
                        #f))
                  (parse-error eq "Expected =")))
            (begin
              (unget-token lexer id)
              (cons 'CONST (reverse clist)))))
      #f))

(define (test-match-const-declaration)
  (test-run "match-const-declaration" match-const-declaration
            '(("const x = 42;"
               (CONST
                ((ID "x" 1) (INT 42 1))))
              ("const x = 42;\n  y = x * 99;"
               (CONST
                ((ID "x" 1) (INT 42 1))
                ((ID "y" 2) (MUL (ID "x" 2) (INT 99 2)))))
              ("const x = 42;\n  y = sin(x);\n  z = sqrt(y) / 3;"
               (CONST
                ((ID "x" 1) (INT 42 1))
                ((ID "y" 2) (CALL (ID "sin" 2) ((ID "x" 2))))
                ((ID "z" 3) (DIV (CALL (ID "sqrt" 3) ((ID "y" 3))) (INT 3 3))))))))

;;; fp-section = [VAR] id-list COLON type
(define (match-fp-section lexer token)
  (parse-trace "match-fp-section" token)
  (define (match-fp-section2 lexer token)
    (let ((id-list (match-id-list lexer token)))
      (if id-list
          (let ((col (get-token lexer)))
            (if (match-token? COLON col)
                (let ((type (match-type lexer (get-token lexer))))
                  (if type
                      (list type id-list)
                      #f))
                (parse-error col "Expected :")))
          #f)))
  (if (match-token? VAR token)
      (let ((fp (match-fp-section2 lexer (get-token lexer))))
        (if fp
            (cons 'VAR fp)
            #f))
      (match-fp-section2 lexer token)))

(define (test-match-fp-section)
  (test-run "match-fp-section" match-fp-section
            '(("i: integer" ((TYPE "integer" 1) ((ID "i" 1))))
              ("var i: integer" (VAR (TYPE "integer" 1) ((ID "i" 1))))
              ("i, j: integer" ((TYPE "integer" 1) ((ID "i" 1) (ID "j" 1))))
              ("var i, j: integer" (VAR (TYPE "integer" 1) ((ID "i" 1) (ID "j" 1)))))))

;;; formal-parameters = OPEN [fp-section {SEMICOLON fp-section}] CLOSE
(define (match-formal-parameters lexer token)
  (parse-trace "match-formal-parameters" token)
  (if (match-token? OPEN token)
      (let loop ((t (get-token lexer)) (fp-list '()))
        (let ((fp-sect (match-fp-section lexer t)))
          (if fp-sect
              (let ((sc (get-token lexer)))
                (if (match-token? SEMICOLON sc)
                    (loop (get-token lexer) (cons fp-sect fp-list))
                    (if (match-token? CLOSE sc)
                        (reverse (cons fp-sect fp-list))
                        (parse-error "Expected )" sc))))
              (if (match-token? CLOSE t)
                  (reverse fp-list)
                  (parse-error t "Expected )")))))
      #f))

(define (test-match-formal-parameters)
  (test-run "match-formal-parameters" match-formal-parameters
            '(("()" ())
              ("(i: integer)" (((TYPE "integer" 1) ((ID "i" 1)))))
              ("(i,j: integer; s: string)"
               (((TYPE "integer" 1) ((ID "i" 1) (ID "j" 1)))
                ((TYPE "string" 1) ((ID "s" 1)))))
              ("(i,j: integer; var b: array 10 of integer)"
               (((TYPE "integer" 1) ((ID "i" 1) (ID "j" 1)))
                (VAR (ARRAY (TYPE "integer" 1) (INT 10 1)) ((ID "b" 1)))))
              ("(i,j: integer; b: array 10 of integer; var c: record x, y: integer end)"
               (((TYPE "integer" 1) ((ID "i" 1) (ID "j" 1)))
                ((ARRAY (TYPE "integer" 1) (INT 10 1)) ((ID "b" 1)))
                (VAR (RECORD ((FIELDS (TYPE "integer" 1) ((ID "x" 1) (ID "y" 1))))) ((ID "c" 1))))))))

;;; procedure-heading = PROCEDURE ID [formal-parameters]
(define (match-procedure-heading lexer token)
  (parse-trace "match-procedure-heading" token)
  (if (match-token? PROCEDURE token)
      (let ((id (get-token lexer)))
        (if (match-token? ID id)
            (let ((fp (match-formal-parameters lexer (get-token lexer))))
              (if fp
                  (list 'PROCEDURE id fp)
                  (list 'PROCEDURE id '())))
            (parse-error id "Expected procedure id")))
      #f))

(define (test-match-procedure-heading)
  (test-run "match-procedure-heading" match-procedure-heading
            '(("procedure hello"
               (PROCEDURE (ID "hello" 1) ()))
              ("procedure add(a, b: integer)"
               (PROCEDURE (ID "add" 1) (((TYPE "integer" 1) ((ID "a" 1) (ID "b" 1))))))
              ("procedure add2(a,b: integer; var d: integer)"
               (PROCEDURE (ID "add2" 1)
                          (((TYPE "integer" 1) ((ID "a" 1) (ID "b" 1)))
                           (VAR (TYPE "integer" 1) ((ID "d" 1)))))))))

;;; procedure-body = declarations [BEGIN statements] END ID
(define (match-procedure-body lexer token)
  (parse-trace "match-procedure-body" token)
  (let ((decl (if (match-token? BEGIN token)
                  (begin (unget-token lexer token) '())
                  (if (match-token? END token)
                      (begin (unget-token lexer token) '())
                      (match-declarations lexer token)))))
    (if decl
        (let ((t (get-token lexer)))
          ;;(display "mpb1 ") (write t) (newline)
          (let ((stats (if (match-token? BEGIN t)
                           (match-statement-list lexer (get-token lexer))
                           (begin
                             (unget-token lexer t)
                             '()))))
            (let ((et (get-token lexer)))
              ;;(display "mpb2 ") (write et) (newline)
              (if (match-token? END et)
                  (let ((it (get-token lexer)))
                    ;;(display "mpb3 ") (write it) (newline)
                    (if (match-token? ID it)
                        (list it decl stats)
                        (parse-error it "Expected procedure ID")))
                  (parse-error et "Expected END")))))
        #f)))

(define (test-match-procedure-body)
  (test-run "match-procedure-body" match-procedure-body
            '(("end test"
               ((ID "test" 1) () ()))
              ("begin\n  a := a + 1\nend test"
               ((ID "test" 3) () ((ASSIGN (ID "a" 2) (ADD (ID "a" 2) (INT 1 2))))))
              ("var a: integer;\nbegin\n  a := a + 1\nend test"
               ((ID "test" 4)
                (#f
                 #f
                 (VAR ((TYPE "integer" 1) ((ID "a" 1))))
                 ())
                ((ASSIGN (ID "a" 3) (ADD (ID "a" 3) (INT 1 3))))))
              ("const x = 12;\nvar a: integer;\nbegin\n  a := a + x\nend test"
               ((ID "test" 5)
                ((CONST ((ID "x" 1) (INT 12 1)))
                 #f
                 (VAR ((TYPE "integer" 2) ((ID "a" 2))))
                 ())
                ((ASSIGN (ID "a" 4) (ADD (ID "a" 4) (ID "x" 4))))))
              ("const x = 12;\ntype name = array 32 of char;\nvar a: integer;\nbegin\n  a := a + x;\n  print(name)\nend test"
               ((ID "test" 7)
                ((CONST ((ID "x" 1) (INT 12 1)))
                 (TYPEDEF ((ID "name" 2) (ARRAY (TYPE "char" 2) (INT 32 2))))
                 (VAR ((TYPE "integer" 3) ((ID "a" 3))))
                 ())
                ((ASSIGN (ID "a" 5) (ADD (ID "a" 5) (ID "x" 5))) (CALL (ID "print" 6) ((ID "name" 6)))))))))

;;; procedure-declaration = procedure-heading SEMICOLON procedure-body SEMICOLON
(define (match-procedure-declaration lexer token)
  (parse-trace "match-procedure-declaration" token)
  (let ((heading (match-procedure-heading lexer token)))
    (if heading
        (let ((t (get-token lexer)))
          (if (match-token? SEMICOLON t)
              (let ((body (match-procedure-body lexer (get-token lexer))))
                (if body
                    (let ((t (get-token lexer)))
                      (if (match-token? SEMICOLON t)
                          (cons heading body)
                          (parse-error t "Expected ;")))
                    #f))
              (parse-error t "Expected ;")))
        #f)))

(define (test-match-procedure-declaration)
  (test-run "match-procedure-declaration" match-procedure-declaration
            '(("procedure add(a, b: integer; var result: integer);\nbegin\n  result := a + b\nend add;"
               ((PROCEDURE
                 (ID "add" 1)
                 (((TYPE "integer" 1) ((ID "a" 1) (ID "b" 1))) (VAR (TYPE "integer" 1) ((ID "result" 1)))))
                (ID "add" 4)
                ()
                ((ASSIGN (ID "result" 3) (ADD (ID "a" 3) (ID "b" 3)))))))))

;;; declarations = [const-declaration] [type-declaration] [var-declaration] {ProcedureDeclaration}
(define (match-declarations lexer token)
  (parse-trace "match-declarations" token)
  (let ((cd (match-const-declaration lexer token)))
    (let ((tt (if cd (get-token lexer) token)))
      (let ((td (match-type-declaration lexer tt)))
        (let ((tv (if td (get-token lexer) tt)))
          (let ((vd (match-var-declaration lexer tv)))
            (let ((tp (if vd (get-token lexer) tv)))
              (let loop ((pd (match-procedure-declaration lexer tp)) (p-list '()))
                (if pd
                    (loop (match-procedure-declaration lexer (get-token lexer)) (cons pd p-list))
                    (begin
                      (unget-token lexer tp)
                      (list cd td vd (reverse p-list))))))))))))

(define (test-match-declarations)
  (test-run "match-declarations" match-declarations
            '((""
               (#f
                #f
                #f
                ()))
              ("const x = 12;"
               ((CONST ((ID "x" 1) (INT 12 1)))
                #f
                #f
                ()))
              ("type name = array 32 of char;"
               (#f
                (TYPEDEF ((ID "name" 1) (ARRAY (TYPE "char" 1) (INT 32 1))))
                #f
                ()))
              ("var a: integer;"
               (#f
                #f
                (VAR ((TYPE "integer" 1) ((ID "a" 1))))
                ()))
              ("const x = 12;\ntype name = array 32 of char;"
               ((CONST ((ID "x" 1) (INT 12 1)))
                (TYPEDEF ((ID "name" 2) (ARRAY (TYPE "char" 2) (INT 32 2))))
                #f
                ()))
              ("const x = 12;\nvar a: integer;"
               ((CONST ((ID "x" 1) (INT 12 1)))
                #f
                (VAR ((TYPE "integer" 2) ((ID "a" 2))))
                ()))
              ("const x = 12;\ntype name = array 32 of char;\nvar a: integer;"
               ((CONST ((ID "x" 1) (INT 12 1)))
                (TYPEDEF ((ID "name" 2) (ARRAY (TYPE "char" 2) (INT 32 2))))
                (VAR ((TYPE "integer" 3) ((ID "a" 3)))) ())))))

;;; module = MODULE ID SEMICOLON declarations [BEGIN statements] END ID DOT
;;; anything after the . is ignored
(define (match-module lexer token)
  (parse-trace "match-module" token)
  (if (match-token? MODULE token)
      (let ((id (get-token lexer)))
        (if (match-token? ID id)
            (let ((sc (get-token lexer)))
              (if (match-token? SEMICOLON sc)
                  (let ((decl (match-declarations lexer (get-token lexer))))
                    (if decl
                        (let ((t (get-token lexer)))
                          (let ((stats (if (match-token? BEGIN t)
                                           (match-statement-list lexer (get-token lexer))
                                           (begin
                                             (unget-token lexer t)
                                             '()))))
                            (let ((et (get-token lexer)))
                              (if (match-token? END et)
                                  (let ((id2 (get-token lexer)))
                                    (if (match-token? ID id2)
                                        (let ((dt (get-token lexer)))
                                          (if (match-token? DOT dt)
                                              (list 'MODULE id id2 decl stats)
                                              (parse-error dt "Expected .")))
                                        (parse-error id2 "Expected module ID")))
                                  (parse-error et "Expected END")))))
                        #f))
                  (parse-error sc "Expected ;")))
            (parse-error id "Expected module ID")))
      (parse-error token "Expected MODULE")))

(define (test-match-module)
  (test-run "match-module" match-module
            '(("module hello;\nbegin\n  print(\"Hello world\") end hello."
               (MODULE (ID "hello" 1)
                       (ID "hello" 3)
                       (#f #f #f ())
                       ((CALL (ID "print" 3) ((STRING "Hello world" 3)))))))))

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
  (test-match-type)
  (test-match-field-list)
  (test-match-record-type)
  (test-match-var-declaration)
  (test-match-type-declaration)
  (test-match-const-declaration)
  (test-match-fp-section)
  (test-match-formal-parameters)
  (test-match-procedure-heading)
  (test-match-declarations)
  (test-match-procedure-body)
  (test-match-procedure-declaration)
  (test-match-module))

(define (parse-file filename)
  (let ((lexer (make-lexer-from-file filename)))
    (let ((result (match-module lexer (get-token lexer))))
      (close-lexer lexer)
      result)))
