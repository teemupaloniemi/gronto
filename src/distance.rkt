#lang racket

(require "io.rkt")
(require "utils.rkt")

;; Sum of the elements in a list.
(define (sum l)
  (define (sum-r l s)
    (if (eq? '() l)
        s
        (sum-r (cdr l) (+ s (car l)))))
  (sum-r l 0))

;; Distance between each outcome.
;; Returns a list of size (length o1) * (length o2).
;; Each element contains:
;;     (source destination distance)
(define (distance-outcomes t o1 o2)
  (let ((cp (cartesian-product o1 o2)))

    ;; Return a path from root of a tree (t) to node (n) if exists.
    ;; Otherwise returns an empty list.
    (define (path t n)
      (define (create-path t n p)
        (cond
          ((not (hash? t)) '())
          ((member n (hash-keys t)) (reverse (cons n p)))
          (else
           (foldr (lambda (k acc)
                    (define sub (create-path (value k t) n (cons k p)))
                    (if (null? sub) acc sub))
                  '()
                  (hash-keys t)))))
      (create-path t n '()))

    ;; Distance from n1 to n2 in o is the length of
    ;;   path from root to n1
    ;; + path from root to n2
    ;; - (2 * common in the paths).
    (define (distance-nodes o n1 n2)
      (let ((p1 (path o n1))
            (p2 (path o n2)))
        ;; Return a list containing elements present in both l1 AND l2.
        (define (common l1 l2)
          (filter (lambda (x) (member x l1))
                  l2))
        (- (+ (length p1) (length p2))
           (* 2 (length (common p1 p2))))))

    (map (lambda (x)
                 (list (car x)
                       (cadr x)
                       (distance-nodes t
                                       (string->symbol (car x))
                                       (string->symbol (cadr x)))))
         cp)))

(define (min-distance-outcomes t o1 o2)
  (let ((d (distance-outcomes t o1 o2)))
    (define (min-of-source source distances)
      (let* ((filtered (filter (lambda (x)
                                  (eq? (car x) source))
                          distances))
             (sorted (sort filtered
                           (lambda (x y)
                                   (< (caddr x) (caddr y))))))
        (if (> (length filtered) 0)
               (caddar sorted)
               999)))

    (define mins-of-sources
      (for*/list ((x o1))
        (min-of-source x d)))
    (/ (sum mins-of-sources) (length mins-of-sources))))

;; Returns a pair of the average length of
;; course outcomes to another outcomes.
;; '(c1 --> c2 c2 --> c1)
(define (distance-courses o c1 c2)
  (let* ((o1 (value 'outcomes c1)) (l1 (length o1))
         (o2 (value 'outcomes c2)) (l2 (length o2)))
    (if (or (eq? l1 0) (eq? l2 0))
        999
        (min-distance-outcomes o o1 o2))))

(define (distance-courses-all courses ontology)
    (map (lambda (p) (list (value 'title (car p))
                           (value 'title (cadr p))
                           (distance-courses ontology (car p) (cadr p))))
         (cartesian-product courses courses)))


(define (prnt fds)
  (define ds (filter (lambda (x) (not (eq? (caddr x) 999))) fds))
  (define weights (map caddr ds))
  (define min-weight (apply min weights))
  (define max-weight (apply max weights))

  ;; Normalized weight [0 (min) to 1 (max)]
  (define (scaled w)
    (/ (- w min-weight) (max 1 (- max-weight min-weight))))

  ;; Color based on original (non-inverted) scaled value
  (define (color w)
    (let ((s (scaled w)))
      (cond ((< s 0.5) "green")
            ((< s 0.66) "blue")
            (else "brown"))))

  ;; Inverted exponential scaling for penwidth
  (define (normalize w)
    (let* ((inv (- 1 (scaled w))) ;; invert so small weights => big penwidth
           (exp-scale (+ 1 (* 4 (exp (* 2 (- inv 1))))))) ; penwidth ~1–5
      exp-scale))

  ;; Print each edge with label, width, and color
  (define (prnt-pair p)
    (let ((w (caddr p)))
      (display "    \"")
      (display (car p)) (display "\" -> \"") (display (cadr p))
      (display "\" [label=\"")
      (display w)
      (display "\" penwidth=")
      (display (normalize w))
      (display ", color=")
      (display (color w))
      (display "];") (newline)))

  (display "digraph Distances {") (newline)
  (map prnt-pair ds)
  (display "}") (newline))

(define dists
  (distance-courses-all (json-read "data/small.json") (json-read "data/acm.json")))

(prnt dists)
