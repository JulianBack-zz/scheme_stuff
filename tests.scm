;;; Tests for lexer and parse

(load "lexer.scm")
(load "parser.scm")

(define (test-lexer)
  (define (test input expected-result)
    (define (get-tokens lexer)
      (let loop ((t (get-token lexer)) (tokens '()))
        (if (eq? 'EOF (car t))
            (reverse tokens)
            (loop (get-token lexer) (cons t tokens)))))
    (let* ((lexer (make-lexer-from-string input))
	       (result (get-tokens lexer)))
      (if (equal? result expected-result)
          (begin
            (write-string input)
            (write-line " OK"))
          (begin
            (write-string input)
            (write-line " FAIL")
            (write result)
            (newline)))))
  (test "procedure test(alpha, beta); (* test function *)\nvar x:integer, y:string, z:char;\nbegin\n x := 42+ alpha ; (* A (* nested *) comment *)\n y := \"Hello, World!\";\n z := 041X;\nend;\n"
        '((PROCEDURE 1) (ID "test" 1) (OPEN 1) (ID "alpha" 1) (COMMA 1) (ID "beta" 1) (CLOSE 1) (SEMICOLON 1)
          (VAR 2) (ID "x" 2) (COLON 2) (ID "integer" 2) (COMMA 2) (ID "y" 2) (COLON 2) (ID "string" 2) (COMMA 2) (ID "z" 2) (COLON 2) (ID "char" 2) (SEMICOLON 2)
          (BEGIN 3)
          (ID "x" 4) (ASSIGN 4) (INT 42 4) (PLUS 4) (ID "alpha" 4) (SEMICOLON 4)
          (ID "y" 5) (ASSIGN 5) (STRING "Hello, World!" 5) (SEMICOLON 5)
          (ID "z" 6) (ASSIGN 6) (CHAR #\A 6) (SEMICOLON 6)
          (END 7) (SEMICOLON 7)))
  (test "0a12h" '((INT 2578 1)))
  ;; Check for errors
  (test "123 (* unmatched comment" '((INT 123 1) (ERROR "EOF in comment" 1)))
  (test "\"hello" '((ERROR "EOF in string" 1)))
  (test "0g12h" '((ERROR "Bad number" 1)))
  (test "?" '((ERROR #\? 1)))
  (test "%" '((ERROR #\% 1)))
  (test "'" '((ERROR #\' 1)))
  (test "$" '((ERROR #\$ 1)))
  (test "{" '((ERROR #\{ 1)))
  (test "}" '((ERROR #\} 1)))
  (test "_" '((ERROR #\_ 1)))
  (test "@" '((ERROR #\@ 1))))

;;; PARSER TESTS

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

(define (test-match-actual-parameters)
  (test-run "match-actual-parameters" match-actual-parameters
            '((")" ())
              ("a" ((ID "a" 1)))
              ("99" ((INT 99 1)))
              ("a, b" ((ID "a" 1) (ID "b" 1)))
              ("1+2, b*c" ((ADD (INT 1 1) (INT 2 1)) (MUL (ID "b" 1) (ID "c" 1))))
              ("1,2,3,4" ((INT 1 1) (INT 2 1) (INT 3 1) (INT 4 1))))))

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

(define (test-match-unary)
  (test-run "match-unary" match-unary
            '(("2" (INT 2 1))
              ("1232" (INT 1232 1))
              ("-45" (MINUS (INT 45 1)))
              ("a" (ID "a" 1))
              ("-b" (MINUS (ID "b" 1)))
              ("-(a+b)" (MINUS (ADD (ID "a" 1) (ID "b" 1)))))))

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

(define (test-match-id-list)
  (test-run "match-id-list" match-id-list
            '(("hello" ((ID "hello" 1)))
              ("hello, world" ((ID "hello" 1) (ID "world" 1)))
              ("a,b,c,d" ((ID "a" 1) (ID "b" 1) (ID "c" 1) (ID "d" 1))))))

(define (test-match-type)
  (test-run "match-type" match-type
            '(("integer" (TYPE "integer" 1))
              ("string" (TYPE "string" 1))
              ("array 10 of integer" (ARRAY (TYPE "integer" 1) (INT 10 1)))
              ("array 10 of array 4 of integer" (ARRAY (ARRAY (TYPE "integer" 1) (INT 4 1)) (INT 10 1))))))

(define (test-match-field-list)
  (test-run "match-field-list" match-field-list
            '(("a: integer" (FIELDS (TYPE "integer" 1) ((ID "a" 1))))
              ("a,b: integer" (FIELDS (TYPE "integer" 1) ((ID "a" 1) (ID "b" 1))))
              ("a,b,c: array 10 of integer" (FIELDS (ARRAY (TYPE "integer" 1) (INT 10 1)) ((ID "a" 1) (ID "b" 1) (ID "c" 1)))))))
                    
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

(define (test-match-fp-section)
  (test-run "match-fp-section" match-fp-section
            '(("i: integer" ((TYPE "integer" 1) ((ID "i" 1))))
              ("var i: integer" (VAR (TYPE "integer" 1) ((ID "i" 1))))
              ("i, j: integer" ((TYPE "integer" 1) ((ID "i" 1) (ID "j" 1))))
              ("var i, j: integer" (VAR (TYPE "integer" 1) ((ID "i" 1) (ID "j" 1)))))))

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

(define (test-match-procedure-declaration)
  (test-run "match-procedure-declaration" match-procedure-declaration
            '(("procedure add(a, b: integer; var result: integer);\nbegin\n  result := a + b\nend add;"
               ((PROCEDURE
                 (ID "add" 1)
                 (((TYPE "integer" 1) ((ID "a" 1) (ID "b" 1))) (VAR (TYPE "integer" 1) ((ID "result" 1)))))
                ()
                ((ASSIGN (ID "result" 3) (ADD (ID "a" 3) (ID "b" 3))))))
              ("procedure add(a, b: integer; var result: integer);\nbegin\n  result := a + b\nend sub;"
               #f))))

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

(define (test-match-module)
  (test-run "match-module" match-module
            '(("module hello;\nbegin\n  print(\"Hello world\") end hello."
               (MODULE (ID "hello" 1)
                       (#f #f #f ())
                       ((CALL (ID "print" 3) ((STRING "Hello world" 3))))))
              ("module hello;\nbegin\n  print(\"Hello world\") end bye."
               #f))))

(define (test-all)
  (test-lexer)
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

(display "Execute (test-all) to run all tests.\n")
