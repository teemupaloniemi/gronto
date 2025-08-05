#lang racket

;; Reading from file.
(require "io.rkt")

;; Data queries.
(require "utils.rkt")

;; Declare INT type variable.
;; Example:
;;     (declare-const sem_ITKA2004 Int)
(define (declare-int code)
  (display "(declare-const sem_") (display code) (display " Int)") (newline))


;; Assert we are in semester limits.
;; Example:
;;     (assert (and (>= sem_ITKA2004 1) (<= sem_ITKA2004 12)))
(define (assert-sem-range code min-sem max-sem)
  (display "(assert (and ")
  (display "(>= sem_") (display code) (display " ") (display min-sem) (display ") ")
  (display "(<= sem_") (display code) (display " ") (display max-sem) (display ")))")
  (newline))


(define (assert-sem-exact code period)
  (display "(= sem_") (display code) (display " ") (display period) (display ")")
  (newline))


;; Declare one course.
(define (decl-sem-var-all code min-sem max-sem)
    (declare-int code)
    (assert-sem-range code min-sem max-sem))


;; Make exact requirement for one of given periods.
(define (decl-sem-var-specific code period)
    (declare-int code)
    (display "(assert (or ")
    (map (lambda (p)
                 (assert-sem-exact code p))
         period)
    (display "))")
    (newline))


;; Declare multiple courses.
(define (decl-sem-vars courses semester-range)
  (let ((min-sem (car semester-range))
        (max-sem (car (reverse semester-range))))
    (map (lambda (c)
                 (if (not (equal? (course-period c) '()))
                   (decl-sem-var-specific (course-code c) (course-period c) )
                   (decl-sem-var-all (course-code c) min-sem max-sem)))
         courses)))


;; Create one prerequisite constraint.
(define (preq-constr-one courses ccode pcode)
  (define (preq-exists)
    (not (equal? (search-by-code courses pcode 'struct) '())))
  (define (show)
    (display "(assert (> sem_") (display ccode)
              (display " sem_") (display pcode)
    (display "))") (newline))
  (define (err)
    (display "; prerequisite ") (display pcode)
    (display " did not exist for course") (display ccode) (newline))
  (if (preq-exists)
      (show)
      (err)))


;; Create all prerequisite constraints.
(define (preq-constr courses)
  (map (lambda (c)
               (map (lambda (p)
                            (preq-constr-one courses (course-code c) p))
                    (course-prerequisite-courses c)))
       courses))


;; Give semester credit constrains to one course.
(define (cred-ite code creds sem)
  (display "        (ite (= sem_")
  (display code)
  (display " ")
  (display sem)
  (display ") ")
  (display creds)
  (display " 0)") (newline))


(define (sum-ites-gt courses sem min-cred-sem)
  (display "(assert (> (+") (newline)
  (map (lambda (c)
               (cred-ite (course-code c) (course-credits c) sem))
       courses)
  (display ") ") (display min-cred-sem) (display "))")
  (newline))


(define (sum-ites-lte courses sem max-cred-sem)
  (display "(assert (<= (+ ") (newline)
  (map (lambda (c)
               (cred-ite (course-code c) (course-credits c) sem))
       courses)
  (display ") ") (display max-cred-sem) (display "))")
  (newline))


(define (cred-constr courses semester-range min-cred-sem max-cred-sem)
  (map (lambda (s)
               (sum-ites-gt courses s min-cred-sem))
       semester-range)
  (map (lambda (s)
               (sum-ites-lte courses s max-cred-sem))
       semester-range))


;; Give total credit constrains. This is almost the same as
;; semester cred. But we sum and compare over all the sems,
;; not individually.
(define (ites courses sem)
  (map (lambda (c)
               (cred-ite (course-code c) (course-credits c) sem))
       courses)
  (newline))


(define (tot-cred-constr courses semester-range min-tot-cred max-tot-cred)
  (display "(assert (>= ")
  (display "(+ ")
  (map (lambda (s)
               (ites courses s))
       semester-range)
  (display ") ") (display min-tot-cred) (display "))") (newline)

  (display "(assert (<= ")
  (display "(+ ")
  (map (lambda (s)
               (ites courses s))
       semester-range)
  (display ") ") (display max-tot-cred) (display "))") (newline))


(define (build-smt-model courses-path
                         years
                         sem-per-year
                         min-tot-cred
                         max-tot-cred
                         min-cred-sem
                         max-cred-sem)
  (let* ((total-semesters (* years sem-per-year))
         (semester-range  (range total-semesters))
         (courses         (hash-to-struct (json-read courses-path))))

  (display "(set-logic QF_LIA)") (newline)
  (decl-sem-vars courses semester-range)
  (preq-constr courses)
  (cred-constr courses
               semester-range
               min-cred-sem
               max-cred-sem)
  (tot-cred-constr courses
                   semester-range
                   min-tot-cred
                   max-tot-cred)
  (display "(check-sat)") (newline)
  (display "(get-model)") (newline)))


(define (main)
  (build-smt-model "tmp/output.json"
                   3     ;; Years
                   4     ;; sem-per-year
                   180   ;; min-tot-cred
                   10000 ;; max-tot-cred
                   5     ;; min-sem-cred
                   20)) ;; max-sem-cred

(main)

