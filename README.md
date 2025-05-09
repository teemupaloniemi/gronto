# Gronto

Gronto is short for `GRaphs and ONTology to Opetussuunnitelma's`. Gronto aims to
use graph theoretic optimization methods for ontology based curriculum planning.
The current idea is to find G, f, H, and M such that:

`f(t, c1, c2) -> d` tells the distance between two courses (c1 and c2) in a given
ontology (t).

`G(t, f, C) -> u` composes a course distance graph (u) such that each edge
between any two courses in the set of all courses (C) is produced by the distance
function (f).

`H(u, l) -> ũ` is a filter removing any edges from source graph (u) below the
threshold value (l).

`M(ũ, s) -> m` creates a model curriculum (m) based on the filtered course
distance graph ũ.

And to create a software implementation that is relatively easy to use at any
platform.

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
```
