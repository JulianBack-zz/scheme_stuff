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

(define (parse-file filename)
  (let ((lexer (make-lexer-from-file filename)))
    (let ((result (match-module lexer (get-token lexer))))
      (close-lexer lexer)
      result)))
