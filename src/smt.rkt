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
(define (decl-sem-var course-code
                      min-sem
                      max-sem)
    (declare-int course-code)
    (assert-sem-range course-code min-sem max-sem))

;; Declare multiple courses.
(define (decl-sem-vars courses
                       semester-range)
  (let ((min-sem (car semester-range))
        (max-sem (car (reverse semester-range))))
    (map (lambda (c)
                 (decl-sem-var (value 'code c) min-sem max-sem))
         courses)))

;; Force prerequisite codes
(define (preq-constr courses)
  (display "; Prerequisite constrainsts")                     (newline)
  (display (value 'code (search-by-code courses "ITKA2004"))) (newline))

;; Give semester credit constrains
(define (cred-constr courses
                     semester-range
                     max-cred-sem
                     min-cred-sem)
  (display "; Credit constraints (semester)") (newline)
  (display semester-range)                    (newline)
  (display max-cred-sem)                      (newline)
  (display min-cred-sem)                      (newline))

;; Give total credit constrains
(define (tot-cred-constr courses
                         semester-range
                         max-tot-cred
                         min-tot-cred)
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

  ;; SMTLIB example
  ;; 1. Header, set logic type.
  (display "(set-logic QF_LIA)") (newline)
  ;; 2. Create vars.
  ;;   (declare-const sem_ITKA2004 Int)
  ;;   (assert (and (>= sem_ITKA2004 1) (<= sem_ITKA2004 12)))
  (decl-sem-vars courses
                 semester-range)
  ;; 3. Constrain prerequisites.
  ;;   (assert (> sem_ITKA201 sem_ITKP102))
  (preq-constr courses)
  ;; 4. Constrain semester min and max limits.
  ;;
  ;;     (assert (<= (+ (ite (= sem_ITKA2004 1) 5 0)
  ;;                    ...
  ;;                    (ite (= sem_TILP2400 1) 5 0))
  ;;                 23))
  ;;     (assert (<= (+ (ite (= sem_ITKA2004 1) 5 0)
  ;;                    ...
  ;;                    (ite (= sem_TILP2400 1) 5 0))
  ;;                 5))
  ;;                     .
  ;;                     .
  ;;                     .
  ;;     (assert (<= (+ (ite (= sem_ITKA2004 12) 5 0)
  ;;                    ...
  ;;                    (ite (= sem_TILP2400 12) 5 0))
  ;;                 23))
  ;;     (assert (<= (+ (ite (= sem_ITKA2004 12) 5 0)
  ;;                    ...
  ;;                    (ite (= sem_TILP2400 12) 5 0))
  ;;                 5))
  (cred-constr courses
               semester-range
               max-cred-sem
               min-cred-sem)
  ;; 5. Constrain total min and max.
  ;;
  ;; (assert (>= (+ (ite (= sem_ITKA2004 1) 5 0)
  ;;                (ite (= sem_ITKA2004 2) 5 0)
  ;;                (ite (= sem_ITKA2004 3) 5 0)
  ;;                (ite (= sem_ITKA2004 4) 5 0)
  ;;                ...
  ;;                (ite (= sem_TILP2400 9) 5 0)
  ;;                (ite (= sem_TILP2400 10) 5 0)
  ;;                (ite (= sem_TILP2400 11) 5 0)
  ;;                (ite (= sem_TILP2400 12) 5 0))
  ;;             180))
  ;; (assert (>= (+ (ite (= sem_ITKA2004 1) 5 0)
  ;;                (ite (= sem_ITKA2004 2) 5 0)
  ;;                (ite (= sem_ITKA2004 3) 5 0)
  ;;                (ite (= sem_ITKA2004 4) 5 0)
  ;;                ...
  ;;                (ite (= sem_TILP2400 9) 5 0)
  ;;                (ite (= sem_TILP2400 10) 5 0)
  ;;                (ite (= sem_TILP2400 11) 5 0)
  ;;                (ite (= sem_TILP2400 12) 5 0))
  ;;             180))
  (tot-cred-constr courses
                   semester-range
                   max-tot-cred
                   min-tot-cred)
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
