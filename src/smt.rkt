#lang racket

;; Reading from file.
(require "io.rkt")

;; Data queries.
(require "utils.rkt")

;; For solvers.
(require rosette)
(require rosette/solver/smt/z3)

(define (build-model solver
                     courses
                     sem-min
                     sem-max
                     min-cred-sem
                     max-cred-sem)
  (let ((sems (for/list ((x (in-range sem-min sem-max))) x))
        ;; These are the SMT symbolic constants.
        (vars (for/list ((c courses))
                (cons c
                  (constant (course-code c)
                            integer?)))))

    (for ((v1 vars))
       ;; Assert semester limits to each course variable.
       (define/contract bs
                       (listof boolean?)
                       (list (and (>= (cdr v1) sem-min)
                                  (<= (cdr v1) sem-max))))
       (solver-assert solver bs)

       ;; Assert that prequisites come later that the course (if they exist).
       (for ((v2 vars))
           ;; When c2 is a member of c1 prerequisites
           (when (member (course-code (car v2))
                         (course-prerequisite-courses (car v1)))
             ;; c2 has to be taken before c1.
             (define/contract bs
                              (listof boolean?)
                              (list (> (cdr v1) (cdr v2))))
             (solver-assert solver bs))))
    ;; Check that each semester has at most/least n credits bound to it.
    (for ((s sems))
      (define/contract bs
                       (listof boolean?)
                       (list (> (apply + (for/list ((v vars))
                                                    (if (= (cdr v) s)
                                                        (course-credits (car v))
                                                        0)))
                                min-cred-sem)))
      (solver-assert solver bs))

    (for ((s sems))
      (define/contract bs
                       (listof boolean?)
                       (list (< (apply + (for/list ((v vars))
                                                    (if (= (cdr v) s)
                                                        (course-credits (car v))
                                                        0)))
                                max-cred-sem)))
      (solver-assert solver bs))

    solver))

(define (build-and-solve courses
                         years
                         sem-per-year
                         min-tot-cred
                         max-tot-cred
                         min-cred-sem
                         max-cred-sem)
    (define solver (z3 'QF_LIA))
    (define mod (build-model solver
                             courses
                             1
                             (* years sem-per-year)
                             min-cred-sem
                             max-cred-sem))
    (define result (solver-check mod))
    (displayln (model result))
    (sat? result))

(define (main)
    (build-and-solve (hash-to-struct (json-read "tmp/output.json"))
                     3     ;; Years
                     4     ;; sem-per-year
                     180   ;; min-tot-cred
                     10000 ;; max-tot-cred
                     0     ;; min-sem-cred
                     20))  ;; max-sem-cred

(main)
