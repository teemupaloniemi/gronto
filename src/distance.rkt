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
;; Returns a list of size (length l1) * (length l2).
(define (cp-path-lengths t l1 l2)
  (let ((cp (cartesian-product l1 l2)))

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

    ;; Create a list of distances related to each node pair in
    ;; cartesian-product.
    (map (lambda (x)
                 (list (car x)
                       (cadr x)
                       (distance-nodes t
                                       (string->symbol (car x))
                                       (string->symbol (cadr x)))))
         cp)))

;; Distance between c1 outcomes and c2 prerequisites in ontology t.
(define (distance t c1 c2)
  (let* ((outs (value 'outcomes c1)) (louts (length outs))
         (pres (value 'outcomes c2)) (lpres (length pres)))

    ;; Distance between two partitions in ontology.
    (define (average-closest-neighbour-distance t outs pres)
        ;; Find closest neighbour of one outcome.
        (define (find-closest-neighbour o g)
          ;; f: Filter ones that have `o` as source.
          ;; s: Sort according to distance in acending order.
          (let* ((f (filter (lambda (x) (eq? (car x) o)) g))
                 (s (sort f (lambda (x y) (< (caddr x) (caddr y))))))
            (if (> (length f) 0)
                (caddar s)
                999)))
        ;; Call closest-neighbour for all outcomes.
        (define closest-neighbours
          (let ((p (cp-path-lengths t outs pres)))
            (for*/list ((oi outs))
              (find-closest-neighbour oi p))))
        ;; Average distance betwee partitions usign the closests neighbours.
        (if (> (length closest-neighbours) 0)
            (/ (sum closest-neighbours) (length closest-neighbours))
            999))

    ;; If ether is empty return large value.
    ;; We cant compute distance to nothing!
    (if (or (eq? outs 0) (eq? pres 0))
        999
        (average-closest-neighbour-distance t outs pres))))

;; Find the distance grpah among the courses according to the distance function f.
;; Save results in triples:
;;     (src dst dist)
(define (G f cs o)
    (map (lambda (p)
                 (list (value 'title (car p))
                       (value 'title (cadr p))
                       (f o (car p) (cadr p))))
         (cartesian-product cs cs)))

(define courses (json-read "data/small.json"))
(define ontology (json-read "data/acm.json"))
(define u (G distance courses ontology))

(define (prnt g)
  (define ds (filter (lambda (x) (not (eq? (caddr x) 999))) g))
  (define weights (map caddr ds))
  (define min-weight (apply min weights))
  (define max-weight (apply max weights))

  ;; Normalized weight [0 (min) to 1 (max)]
  (define (scaled w)
    (/ (- w min-weight) (max 1 (- max-weight min-weight))))

  ;; Color based on original (non-inverted) scaled value
  (define (color w)
    (let ((s (scaled w)))
      (cond ((< s 0.45) "green")
            ((< s 0.55) "yellow")
            ((< s 0.65) "orange")
            (else "brown"))))

  ;; Print each edge with label, width, and color
  (define (prnt-pair p)
    (let ((w (caddr p)))
      (display "    \"")
      (display (car p)) (display "\" -> \"") (display (cadr p))
      (display "\" [label=\"")
      (display w)
      (display "\" penwidth=2")
      (display ", color=")
      (display (color w))
      (display ", style=dashed")
      (display "];") (newline)))

  (display "digraph Distances {") (newline)
  (map prnt-pair ds)
  (display "}") (newline))

(prnt u)
