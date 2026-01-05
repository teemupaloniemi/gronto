#lang racket
(require racket/cmdline)
(require graph)
(require "graphviz.rkt")
(require "utils.rkt")
(require "smt.rkt")


(define (symbolic->string symbolic)
  (format "~s" symbolic))


(define (gen-dot courses max-sem sem-pairs outputport)
  (define adj (map (lambda (c) (cons (course-code c)
                                     (course-prerequisite-courses c)))
                   courses))
  (define g (unweighted-graph/adj adj))
  (define-vertex-property g
                          semester #:init
                          "not-set")
  (define-vertex-property g
                          id #:init
                          "not-set")
  (define-vertex-property g
                          label
                          #:init
                          "not-set")
  (for ((c sem-pairs))
       (semester-set! (symbolic->string (car c))
                      (cdr c))
       (id-set! (symbolic->string (car c))
                (symbolic->string (car c)))
       (label-set! (symbolic->string (car c))
                   (course-name (search-by-code courses
                                                (symbolic->string (car c))
                                                'struct))))
  (mygraphviz g
              id
              semester
              max-sem
              #t
              #:output outputport
              #:graph-attributes (list (list 'rankdir "TB")
                                       (list 'ranksep 1))
              #:vertex-attributes (list (list 'semester semester)
                                        (list 'label label))))


(define (safe-gen-dot courses max-sem sem-pairs outputport)
  (define (error-message)
    (raise "Error: could not solve for model"))
  (if (equal? sem-pairs '())
      (error-message)
      (gen-dot courses
               max-sem
               sem-pairs
               outputport))
  #t)


(define (main args)
  (define courses (hash-to-struct (json-read (vector-ref args 0))))
  (define outputport (open-output-file (vector-ref args 1)  #:exists 'replace))
  (define years (string->number (vector-ref args 2)))
  (define sems (string->number (vector-ref args 3)))
  (define min-cred (string->number (vector-ref args 4)))
  (define max-cred (string->number (vector-ref args 5)))
  (define schedule (build-and-solve courses
                                    years
                                    sems
                                    min-cred
                                    max-cred))
  (safe-gen-dot courses
                (* years
                   sems)
                schedule
                outputport))


(main (current-command-line-arguments))
