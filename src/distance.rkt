#lang racket

;; Shortest path algorithm.
(require graph)
;; For saving precomputed data.
(require racket/serialize)
;; Mean
(require math/statistics)
;; Reading from file.
(require "io.rkt")
;; Data queries.
(require "utils.rkt")
;; For precomputed data.
(require "precomputed.rkt")
;; For the ontology.
(require "ontology.rkt")

;; Constant for edges that are infinite.
(define INF +inf.0)

;; Write data and return the data back.
(define (write-precomputed data)
  (displayln "Writing precomputed data.")
  (call-with-output-file "src/precomputed.rkt"
    (lambda (out)
      (fprintf out "#lang racket\n\n(provide precomputed)\n(define precomputed\n  ~s)\n" data))
    #:exists 'replace)
  data)


;; A wrapper for deciding which way we get the distance map.
;; If a precomputed file exists we load it. Otherwse the values
;; are computed, saved to a precomputed file and returned.
(define all-pair-distances (if (hash? precomputed)
                               precomputed
                               (write-precomputed (johnson (unweighted-graph/adj ontology)))))


;; Distance of two ontology nodes n1 and n2 in distance pairs p.
(define (f pair)
  (define (weight-factor ow pw)
    (if (< ow pw)
      (- 1
         (/ (sqr (abs (- ow pw)))
            (sqr 6))) ;; 6 is the maximum.
      1))

  ;; TODO: Problems will araise if o == p, because then
  ;;       the hash-ref will return 0 but weight factor
  ;;       might imply that expectations between courses
  ;;       don't align. Needs a fix!
  (define (distance o p)
    (* (weight-factor (cadr o)
                      (cadr p))
       (hash-ref all-pair-distances
                 (list (string->symbol (car o))
                       (string->symbol (car p))))))
  (list (caar pair)
        (caadr pair)
        (distance (car pair) (cadr pair))))


;; Distance between one course (c1) outcomes and another (c2) prerequisites
;; by function (f).
(provide G)
(define (G outs pres)
  (define (closest)
    (let ((p (map f (cartesian-product outs pres))))
      (for/list ((o outs))
        (let ((sorted (sort (filter (lambda (x) (equal? (car x) (car o)))
                                     p)
                            (lambda (x y) (< (caddr x) (caddr y))))))
          (if (> (length sorted) 0)
              (caddar sorted)
              INF)))))
  (if (and (> (length outs) 0) (> (length pres) 0))
      (mean (closest))
      INF))


(provide D)
(define (D c1 c2)
  ;; For each pair
  (list (course-code c1)
        (course-code c2)
        ;; Compute the distance and weight by course credits.
        (* (mean (mean (course-credits c1))
                 (mean (course-credits c2)))
           (G (course-skill-outcomes c1)
              (course-skill-prerequisites c2)))))


;; TODO: Could consider making bidirectionals a one component. If two courses
;; depend on each other make all future courses that depend on either one
;; depend on both of them and remove the bidirectional. This produces less cycles.
(provide H)
(define (H u th)
  ;; Filter any edges below threshold value.
  (define f (filter (lambda (x) (< (caddr x) th)) u))

  ;; Remove larger/worse/less prerequisite of bidirectional edges.
  (define (remove-bidirectionals current)
    (let* ((r (filter (lambda (new) (and (equal? (car  new) (cadr current))
                                         (equal? (cadr new) (car  current))))
                      f))
           (other (if (> (length r) 0) (car r) '())))

      ;; Find shorter. TODO: Make beautiful with cond. This is hard to read.
      (if (not (equal? '() other))
          (if (< (caddr current) (caddr other))
              ;; Current order is best.
              current
              (if (equal? (caddr current) (caddr other))
                 ;; Use name for predictable behaviour Since it does not really
                 ;; matter which way we choose.
                 (if (string>? (car current) (cadr current))
                     current
                     other)
                 other))
          ;; Only one order.
          current)))

  (remove-duplicates (for*/list ((p f))
                       (remove-bidirectionals p))))


;; Visualize :)
(define (print-dot-graph edges courses port)
  ;; Print each edge with label, width, and color
  (define (conditional-print-edge p)
    (let* ((src-course (search-by-code courses (car p) 'struct))
           (dst-course (search-by-code courses (cadr p) 'struct))
           (src-name (course-name src-course))
           (dst-name (course-name dst-course))
           (w (caddr p)))
      (define (print-edge)
        (display "    \"")
        (display src-name) (display "\" -> \"") (display dst-name)
        (display "\" [label=\"")
        (display w)
        (display "\"")
        (display ", style=dashed")
        (display "];")
        (newline))
      (when (not (equal? src-name dst-name))
        (print-edge))))

  (map conditional-print-edge edges))

;; Assign prerequsite courses to courses and save them for scheduling.
;; TODO: Get rid of course-hashes.
(define (save-results filename graph course-hashes course-structs)
  ;; Get a list of all known course codes.
  (define course-codes
    (map (lambda (c) (course-code c))
         course-structs))

  ;; Find all edges that point to node with given code.
  (define (get-prerequisites code)
    (map car
         (filter (lambda (x) (and (not (eqv? (car x)
                                             code))
                                  (eqv? (cadr x)
                                        code)))
                 graph)))

  ;; Mutate the original course by adding a list of prerequisite-courses.
  (define (assign-prerequisites code)
    (define mutable (hash-copy (search-by-code course-hashes code 'hash)))
    (hash-set! mutable
               'course-prerequisites
               (get-prerequisites code))
    mutable)

  ;; Finally write to a file.
  (json-write filename (map assign-prerequisites course-codes)))


(define (main)
  ;; Courses hash is still needed as we will mutate and save them later.
  ;; Specifically we sill assign prerequisite courses based on our computation.
  (define course-hashes  (json-read "data/input.json"))
  (define course-structs (hash-to-struct course-hashes))
  (define g (H (map (lambda (p) (D (car p) (cadr p)))
                    (cartesian-product course-structs
                                       course-structs))
               1000))
  (when (> (length g) 0)
      (print-dot-graph g course-structs "tmp/distance.dot"))
  (save-results "tmp/output.json" g course-hashes course-structs))

(main)
