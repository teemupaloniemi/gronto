#lang racket
(require "io.rkt")
(require "utils.rkt")

;; Declare INT type variable.
;; Example:
;;     (declare-const sem_ITKA2004 Int)
(define (declare-int course-code)
  (display "(declare-const sem_") (display course-code) (display " Int)") (newline))

;; Assert we are in semester limits.
;; Example:
;;     (assert (and (>= sem_ITKA2004 1) (<= sem_ITKA2004 12)))
(define (assert-sem-range course-code min-sem max-sem)
  (display "(assert (and ")
  (display "(>= sem_") (display course-code) (display " ") (display min-sem) (display ") ")
  (display "(<= sem_") (display course-code) (display " ") (display max-sem) (display ")))")
  (newline))

;; Declare one course.
(define (decl-sem-var course-code min-sem max-sem)
    (declare-int course-code)
    (assert-sem-range course-code min-sem max-sem))

;; Declare multiple courses.
(define (decl-sem-vars courses semester-range)
  (let ((min-sem (car semester-range))
        (max-sem (car (reverse semester-range))))
    (map (lambda (c)
                 (decl-sem-var (value 'code c) min-sem max-sem))
         courses)))

;; Create one prerequisite constraint.
(define (preq-constr-one course-code preq-code)
  (display "(assert (> sem_") (display course-code)
            (display " sem_") (display preq-code)
  (display "))") (newline))

;; Create all prerequisite constraints.
(define (preq-constr courses)
  (map (lambda (c)
               (map (lambda (p)
                            (preq-constr-one (value 'code c) p))
                    (value 'prerequisites c)))
       courses))

;; Give semester credit constrains to one course.
(define (cred-ite course-code course-creds sem)
  (display "        (ite (= sem_")
  (display course-code)
  (display " ")
  (display sem)
  (display ") ")
  (display course-creds)
  (display " 0)") (newline))

(define (sum-ites-gt courses sem min-cred-sem)
  (display "(assert (> (+") (newline)
  (map (lambda (c)
               (cred-ite (value 'code c) (value 'credits c) sem))
       courses)
  (display ") ") (display min-cred-sem) (display "))")
  (newline))

(define (sum-ites-lte courses sem max-cred-sem)
  (display "(assert (<= (+ ") (newline)
  (map (lambda (c)
               (cred-ite (value 'code c) (value 'credits c) sem))
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

;; Give total credit constrains.
(define (tot-cred-constr courses semester-range max-tot-cred min-tot-cred)
  (display "; Credit constraints (total)") (newline)
  (display semester-range)                 (newline)
  (display max-tot-cred)                   (newline)
  (display min-tot-cred)                   (newline))

;;;; Building the actual model ;;;;

(define (build-smt-model courses-path
                         years
                         sem-per-year
                         min-tot-cred
                         max-tot-cred
                         min-cred-sem
                         max-cred-sem)
  (let* ((total-semesters (* years sem-per-year))
         (semester-range  (range total-semesters))
         (courses         (json-read courses-path)))

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
  ;; 6. Close the model.
  (display "(check-sat)") (newline)
  (display "(get-model)") (newline)))

;; Call the builder.
(build-smt-model "../data/tekka.json"
                 3    ;; Years
                 4    ;; sem-per-year
                 180  ;; min-tot-cred
                 1000 ;; max-tot-cred
                 2    ;; min-sem-cred
                 23)  ;; max-sem-cred
