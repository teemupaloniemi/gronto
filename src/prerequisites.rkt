#lang racket
(require racket/cmdline)
(require graph)
(require racket/serialize)
(require math/statistics)
(require "io.rkt")
(require "utils.rkt")
(require "precomputed.rkt")
(require "ontology.rkt")
(require "graphviz.rkt")


(define INF +inf.0)


(define (write-precomputed data)
  (call-with-output-file "src/precomputed.rkt"
    (lambda (out)
      (fprintf out "#lang racket\n\n(provide precomputed)\n(define precomputed\n  ~s)\n" data))
    #:exists 'replace)
  data)


(define all-pair-distances
  (if (hash? precomputed)
      precomputed
      (write-precomputed (johnson (unweighted-graph/adj ontology)))))


(define (remove-bidirectionals one all)
  (let* ((f (filter (lambda (new) (and (equal? (car  new)
                                               (cadr one))
                                       (equal? (cadr new)
                                               (car  one))))
                    all))
         (two (if (> (length f)
                       0)
                    (car f)
                    '())))
    (if (not (equal? '()
                     two))
        (if (< (caddr one)
               (caddr two))
            one
            (if (equal? (caddr one)
                        (caddr two))
               (if (string>? (car one)
                             (cadr one))
                   one
                   two)
               two))
        one)))


(define (H u th)
  (let ((û (filter (lambda (x) (< (caddr x) th))
                   u)))
  (remove-duplicates (for/list ((p û))
                       (remove-bidirectionals p
                                              û)))))


(define (get-prerequisites code graph)
  (map car
      (filter (lambda (x) (and (not (eqv? (car x)
                                          code))
                               (eqv? (cadr x)
                                     code)))
              graph)))


(define (mutate-prerequisites course-hashes code graph)
  (let ((mutable (hash-copy (search-by-code course-hashes
                                            code
                                            'hash))))
    (hash-set! mutable
              'course-prerequisites
              (get-prerequisites code
                                 graph))
    mutable))


(define (save-results filename graph course-hashes course-structs)
  (json-write filename
              (map (lambda (c) (mutate-prerequisites course-hashes
                                                     (course-code c)
                                                     graph))
                   course-structs)))


;; Code : C --> S
(define (Code c)
  (course-code c))


;; Cred : C --> N
(define (Cred c)
  (course-credits c))


;; P : C --> (W x N)*
(define (P c)
  (course-skill-prerequisites c))


;; O : C --> (W x N)*
(define (O c)
  (course-skill-outcomes c))


(define (bloom-difference-weight out-bloom pre-bloom)
  (if (< out-bloom
         pre-bloom)
      (let ((m 6)) ;; The maximum difference is 6.
        (/ (- m
              (- pre-bloom
                 out-bloom))
           m))
      1))


(define (ontology-distance node-1 node-2)
  (hash-ref all-pair-distances
            (list (string->symbol node-1)
                  (string->symbol node-2))))


;; shortest-pair : (N x N x Q)* x N --> Q
;; Returns:
;;   Shortest of the triplets containing "one".
(define (shortest-pair distances one)

  ;; Consider only pairs with "one" in outcomes.
  (define f (filter (lambda (x) (equal? (car x)
                                        (skill-name one)))
                    distances))

  ;; Sort them in "shortest first" order.
  (define s (sort f
                  (lambda (x y) (< (car (reverse x))
                                   (car (reverse y))))))

  ;; Return the shortest.
  (if (equal? s '())
      INF
      (car (reverse (car s)))))


;; f : (N x W) x (N x W) --> Q
;; Returns:
;;   Distance between the two ontology nodes.
(define (f node-1 node-2)
  (* (bloom-difference-weight (skill-bloom node-1)
                              (skill-bloom node-2))
     (ontology-distance (skill-name node-1)
                        (skill-name node-2))))


;; fs : (N x W)* x (N x W)* --> (N x N x Q)*
;; Returns:
;;   Distances between pairs of ontology nodes (with associated nodes).
(define (fs outcomes prerequisites)
  (map (lambda (p) (list (skill-name (car p))
                         (skill-name (cadr p))
                         (f (car p)
                            (cadr p))))
       (cartesian-product outcomes
                          prerequisites)))


;; closest : (N x W)* x (N x W)* --> Q*
;; Returns:
;;   List of distances (one for each outcome).
(define (closest outcomes prerequisites)

  ;; Get all distances.
  (define distances (fs outcomes
                        prerequisites))

  ;; Return the shortest for each outcome.
  (map (lambda (o) (shortest-pair distances
                                  o))
       outcomes))


;; G : (N x W)* x (N x W)* --> Q
;; Returns:
;;   Distance between two lists of weighted ontology nodes.
(define (G outcomes prerequirements)
  (if (and (> (length outcomes)
              0)
           (> (length prerequirements)
              0))
      (mean (closest outcomes
                     prerequirements))
      INF))


;; D : C x C --> (S x S x Q)
;; Returns:
;;  Distance between two courses (and names associated).
(define (D course-1 course-2)
  (list (Code course-1)
        (Code course-2)
        (* (mean (mean (Cred course-1))
                 (mean (Cred course-2)))
           (G (O course-1)
              (P course-2)))))


(define (main args)

  ;; Input arguments from command line.
  (define input-file       (vector-ref args 0))
  (define graph-file       (vector-ref args 1))
  (define output-file      (vector-ref args 2))
  (define filter-threshold (string->number (vector-ref args 3)))

  ;; Computation
  (define course-hashes  (json-read input-file))
  (define course-structs (hash-to-struct course-hashes))
  (define graph (H (map (lambda (p) (D (car p)
                                   (cadr p)))
                        (cartesian-product course-structs
                                           course-structs))
                   filter-threshold))

  ;; Visualize
  (when (> (length graph)
           0)
      (print-dot-graph graph
                       course-structs
                       graph-file))

  ;; Save results
  (save-results output-file
                graph
                course-hashes
                course-structs))


(main (current-command-line-arguments))
