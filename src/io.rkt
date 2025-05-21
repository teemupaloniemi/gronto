#lang racket
(require json)

;; Read the courses from a given file.
(provide json-read)
(define (json-read filename)
    (call-with-input-file filename read-json))


;; Writing some course data in a file.
(provide json-write)
(define (json-write filename json)
  (call-with-output-file filename
    (λ (x) (write-json json x))
    #:exists 'replace))


;; Reading a string from SMT output.
;; Parsing this is done at dot.rkt -file.
(provide smt-read)
(define (smt-read filename)
  (port->string (open-input-file filename) #:close? #t))
