# Gronto

Gronto is short for `GRaphs and ONTology to Opetussuunnitelma's`. Gronto aims to
use graph theoretic optimization methods for ontology based curriculum planning.
The current idea is to find G, f, H, and M such that:

## Constructing a distance graph ([distance.rkt](src/distance.rkt))

### Distance function

`f(t, c1, c2) -> d` tells the distance between two courses (c1 and c2) in a given
ontology (t).

For example for two courses:

```
C1:
    outcome1
    outcome2
C2:
    outcome1
    outcome3
    outcome4
```

We have `f` such that is computes the average distance between the nodes of
c1 outcomes from c2 outcomes in the ontology-tree. The steps for this are:

 1. Create a cartesian-product of c1 and c2 outcomes (we will add prerequisites later).
 ```
 ("outcome1" "outcome1")
 ("outcome1" "outcome3")
 ("outcome1" "outcome4")
 ("outcome2" "outcome1")
 ("outcome2" "outcome3")
 ("outcome2" "outcome4")
 ```

 2. Count distance for each pair. This is done by counting taking the paths from
 root to outcome-a and root to outcome-b. Then common part is discarded from least
 ancestor (LCA) and tail lengths are added (see code below).

 ```scheme
 (define (distance-nodes o n1 n2)
   (let ((p1 (path-from-root o n1))
         (p2 (path-from-root o n2)))

     ;; Return a list containing elements present in both l1 AND l2.
     (define (common l1 l2)
       (filter (lambda (x) (member x l1))
               l2))

     (- (+ (length p1) (length p2))
        (* 2 (length (common p1 p2))))))
 ```

 Resulting in this:
 ```
 ("outcome1" "outcome1" 0)
 ("outcome1" "outcome3" 7)
 ("outcome1" "outcome4" 16)
 ("outcome2" "outcome1" 3)
 ("outcome2" "outcome3" 8)
 ("outcome2" "outcome4" 1)
 ```

 3. Select the minimum for each where source is c1 outcome.
 ```
 ("outcome1" "outcome1" 0)
 ("outcome2" "outcome4" 1)
 ```

 4. Count the average of these.
 ```
 (0 + 1) / 2 = 1/2
 ```

With this the distance to course itself is `0` because each outcome finds minimum
from its counterpart which is 0 for all.

### Map to all courses

`G(t, f, C) -> u` composes a course distance graph (u) such that each edge
between any two courses in the set of all courses (C) is produced by the distance
function (f).

```scheme
(define (G t f C)
    (map (lambda (p)
                 (list (value 'title (car p))
                       (value 'title (cadr p))
                       (f t (car p) (cadr p))))
         (cartesian-product C C)))
```

### Filter

`H(u, th) -> ũ` is a filter removing any edges from source graph (u) below the
threshold value (th).

```scheme
(define (H u th)
  (filter (lambda (x) (< (caddr x) th))
          u))
```

(This all can be visualized with `make dist`.)

## Schedule ([smt.rkt](src/smt.rkt) and [dot.rkt](src/dot.rkt))

`M(ũ, s) -> m` creates a model curriculum (m) based on the filtered course
distance graph ũ. (A rought idea of this can be visualized with `make all`).

## Usage

```
# Install z3 theorem solver.

apt install z3

# Install racket.

add-apt-repository ppa:plt/racket
apt-get install racket

# Run scripts (ba).
make

# Run scripts (ba+ms).
LARGE=1 make

# Distance
make dist
```
