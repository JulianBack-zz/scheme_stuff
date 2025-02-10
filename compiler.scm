(import (chicken process-context))

(load "lexer.scm")
(load "parser.scm")

(define (compile files)
  (if (not (null? files))
      (begin
        (write (parse-file (car files)))
        (newline)
        (compile (cdr files)))))

(compile (command-line-arguments))
