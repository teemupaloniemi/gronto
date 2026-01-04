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
         (other (if (> (length f)
                       0)
                    (car f)
                    '())))

    (if (not (equal? '()
                     other))
        (if (< (caddr one)
               (caddr other))
            one
            (if (equal? (caddr one)
                        (caddr other))
               (if (string>? (car one)
                             (cadr one))
                   one
                   other)
               other))
        one)))


(define (H u th)
  (let ((f (filter (lambda (x) (< (caddr x) th))
                   u)))
  (remove-duplicates (for/list ((p f))
                       (remove-bidirectionals p
                                              f)))))


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



(define (bloom-difference-weight out-bloom prereq-bloom)
  (if (< out-bloom
         prereq-bloom)
      (let ((m 6)) ;; maximum difference
        (/ (- m
              (- prereq-bloom
                 out-bloom))
           m))
      1))


(define (competence-distance n1 n2)
  (hash-ref all-pair-distances
            (list (string->symbol n1)
                  (string->symbol n2))))


(define (distance o p)
  (* (bloom-difference-weight (cadr o)
                              (cadr p))
     (competence-distance (car o)
                          (car p))))


(define (fs outs pres)
  (map f
       (cartesian-product outs
                          pres)))


(define (sort-pairs pairs one)
  (sort (filter (lambda (x) (equal? (car x)
                                    (car one)))
                pairs)
        (lambda (x y) (< (caddr x)
                         (caddr y)))))


(define (closest outs pres)
  (let ((pairs (fs outs
                   pres)))
    (for/list ((one outs))
      (let ((sorted (sort-pairs pairs
                                one)))
        (if (> (length sorted)
               0)
            (caddar sorted)
            INF)))))



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


;; f : (N x W)^2 --> (N x N x Q)
(define (f pair)
  (list (caar pair)
        (caadr pair)
        (distance (car pair)
                  (cadr pair))))


;; G : (N x W)* x (N x W)* --> Q
(define (G outs pres)
  (if (and (> (length outs)
              0)
           (> (length pres)
              0))
      (mean (closest outs
                     pres))
      INF))


;; D : C x C --> (S x S x Q)
(define (D c1 c2)
  (list (Code c1)
        (Code c2)
        (* (mean (mean (Cred c1))
                 (mean (Cred c2)))
           (G (O c1)
              (P c2)))))


(define (main args)
  (define course-hashes (json-read (vector-ref args
                                               0)))
  (define course-structs (hash-to-struct course-hashes))
  (define g (H (map (lambda (p) (D (car p)
                                   (cadr p)))
                    (cartesian-product course-structs
                                       course-structs))
               (string->number (vector-ref args
                                           3))))
  (when (> (length g)
           0)
      (print-dot-graph g
                       course-structs
                       (vector-ref args
                                   1)))
  (save-results (vector-ref args
                            2)
                g
                course-hashes
                course-structs))


(main (current-command-line-arguments))
