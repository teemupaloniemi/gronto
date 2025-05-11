# Gronto

Gronto is short for `GRaphs and ONTology to Opetussuunnitelma's`. Gronto aims to
use graph theoretic optimization methods for ontology based curriculum planning.
The current idea is to find G, f, H, and M such that:

## Distance

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

So far we have an f such that is measures the average of the minimum distance from
each c1 outcome to c2 outcome. The steps for this are:

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
 ancestor (LCA) and tail lengths are added.
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

`G(t, f, C) -> u` composes a course distance graph (u) such that each edge
between any two courses in the set of all courses (C) is produced by the distance
function (f). (This can be visualized with `make dist`.)

## Filter

`H(u, l) -> ũ` is a filter removing any edges from source graph (u) below the
threshold value (l).

## Schedule

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
