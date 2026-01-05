#lang racket
(require (only-in racket/base (eq? eqt?)))
(require racket/cmdline)
(require graph)
(require math/statistics)
(require rosette/safe)
(require rosette/solver/smt/z3)
(require "utils/graphviz.rkt")
(require "utils/utils.rkt")

(define (build-model solver courses sem-min sem-max min-cred-sem max-cred-sem)
  (let ((sems (for/list ((x (in-range sem-min (+ sem-max 1)))) x))
        ;; These are the SMT symbolic constants.
        (vars (for/list ((c courses))
                        (cons c (constant (course-code c)
                                          integer?)))))

    (for ((v1 vars))
      ;; Assert semester limits to each course variable.
      (define bs1 (list (and (>= (cdr v1) sem-min)
                             (<= (cdr v1) sem-max))))
      (solver-assert solver bs1)

      ;; If strict period constraints exists, apply them.
      ;; I.e course is kept in first semster of each year
      ;; and therefore to be in semester 1, 5 and 9.
      (when (not (equal? (course-periods (car v1)) '()))
        (for ((p (course-periods (car v1))))
             (solver-assert solver (list (equal? (cdr v1) p)))))

      ;; Assert that prequisites come later that the course (if they exist).
      (for ((v2 vars))
        ;; When c2 is a member of c1 prerequisites
        (when (member (course-code (car v1))
                      (course-prerequisite-courses (car v2)))
          ;; c2 has to be taken before c1.
          (define bs3 (list (> (cdr v2) (cdr v1))))
          (solver-assert solver bs3))))

    ;; Check that each semester has at most/least n credits bound to it.
    (for ((s sems))
      (define bs (list (and (>= (apply + (for/list ((v vars))
                                                   (if (= (cdr v) s)
                                                     (mean (course-credits (car v)))
                                                     0)))
                                min-cred-sem)
                            (<= (apply + (for/list ((v vars))
                                                   (if (= (cdr v) s)
                                                     (mean (course-credits (car v)))
                                                     0)))
                                max-cred-sem))))
      (solver-assert solver bs))

    solver))

(provide build-and-solve)
(define (build-and-solve courses years sem-per-year min-cred-sem max-cred-sem)
  (define solver (z3 'QF_LIA))
  (define mod (build-model solver
                           courses
                           1
                           (* years
                              sem-per-year)
                           min-cred-sem
                           max-cred-sem))
  (define result (solver-check mod))
  (hash->list (model result)))


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

  ;; Command line arguments
  (define input-file (vector-ref args 0))
  (define graph-file (vector-ref args 1))
  (define years (string->number (vector-ref args 2)))
  (define sems (string->number (vector-ref args 3)))
  (define min-cred (string->number (vector-ref args 4)))
  (define max-cred (string->number (vector-ref args 5)))

  ;; Data and IO
  (define courses (hash-to-struct (json-read input-file)))
  (define outputport (open-output-file graph-file
                                       #:exists
                                       'replace))

  ;; Computation
  (define schedule (build-and-solve courses
                                    years
                                    sems
                                    min-cred
                                    max-cred))

  ;; Visualization
  (safe-gen-dot courses
                (* years
                   sems)
                schedule
                outputport))


(main (current-command-line-arguments))
