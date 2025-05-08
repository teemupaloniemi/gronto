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
