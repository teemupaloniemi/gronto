# What?

Gronto is short for `GRaphs and ONTology to Opetussuunnitelma`. Gronto aims to
bring the [ACM Compute Classification System's](https://dl.acm.org/ccs)
benefits to curriculum design. We also touch some topics from [graph
theory](https://en.wikipedia.org/wiki/Graph_theory), and [satisfiability
solvers](https://en.wikipedia.org/wiki/Satisfiability_modulo_theories).

The goal is to move towards a computable curriculum planning process by making
a software that constructs curriculum automatically just from course syllabi.
We focus specifically to the field of computer science. We do not assume to
replace the current meeting oriented process but rather make it a bit more
effective by fixing some problems. We like to think it as replacing old
bearings to make a car or bike a bit more efficient. Below we first list some
definitions and after that the problems we see and give a brief explanation on
why our solution solves the issue.

---

# Definitions

***The group*** A curriculum is traditionally build by a group of teachers and
other staff. A problems arise as the people are selected for this group with
the lowest bidder philosophy -- in terms of available time. Surely this group
does not represent the university and its goals (optimally). Still this group
is essentially responsible for the quality of almost half of the university's
operations: teaching. Our proposal does not account for the selection problem
but similar ideas could be used for skill-to-people mapping and group
formation.

***Course*** For out purposes the below format consists a course. We do
understand that a course includes much more but things like lecture plans,
material, descriptions, assesment and others do not interest us (yet).

```
  code:          TEST123
  title:         Course name
  credits:       5
  prerequisites: P1 ... PN
  outcomes:      O1 ... OM
```

Here Ps and Os are selected from the ACM Compute Classification System.

---

\clearpage


# Problems

***Aligning to reality*** As the group's first task, several meetings are used
on discussing the meaning of some esoteric ideas like *What does the term
"software engineer" mean?*, *Do software engineers only program?* or *What is
programming actually about?*. This phase is quite important as it gives birth
to the main topics that the curriculum must cover. Some topics might be
*Software creation and management, Theory of computation, Security and privacy,
Networks, ect.* These topics ensure that the curriculum is aligned to reality
and the future academics and professional are focused on the most pressing
issues of the time.

The current process produces biased results and does so slowly. We can speed
up and unbias this phase by relying on previously documented topics that are
checked to represent the field of computer science. ACM Compute Classification
System does exactly this.

> *[ACM classification system has been] the de facto standard
> classification system for the computing field. ... It relies
> on a semantic vocabulary as the single source of categories
> and concepts that reflect the state of the art of the computing
> discipline and is receptive to structural change as it evolves
> in the future.*

***Tedious and error prone work*** Then the group hand picks all courses from
the university's course catalog which might cover those topics. This means
reading hundreds of course syllabi and trying to reason under which topic they
fall. This is tedious and error prone work as it depends solely on the
interpretation of the syllabus.

By restricting the language used in the syllabus to an ontology we gain
certainty on the meaning of the contents. For example consider a course
syllabus which states

> Students learn *abstraction* in this course.

We don't really know what is meant by that. We might have an idea when we read
the course title and some background material but that is exactly the kind of
tedious and error prone work we try to avoid. Now consider using the ACM
Compute Classification System. The term abstraction would be replaced by one of
the following.

> *Theory of computation &rarr; Logic &rarr; Abstraction*

> *General programming languages &rarr; Language features &rarr; Abstract data types*

> *Software organization and properties &rarr; Software system structures &rarr; Abstraction, modeling and modularity*

Now its right away clear what is meant and there is no need to dig further
information from the course syllabus.

***Ordering*** One problem that the "topic-ing" strategy still does not solve is
the order in which courses must be attended. The topics usually don't have any
ordered relation to each other. Suppose you have to learn about *Networks* and
*Security and privacy*. Which one should you learn first? What about *Theory of
computation* and *Software and its engineering*? There might be opinions about
the answer but opinion based reasoning is a bit too ambiguous. That is what the
group currently does. We would like easily computable stuff.

When the ontology is selected such that it is a finite network we can indeed
start to process the information using computable algorithms. Two terms from
the ontology network have a distance from each other that we can compute.
Therefore any prerequisite and outcome pair has a distance that we can compute.
With a bit more graph theory we can compute a distance between all pairs of
courses, in both directions. This means we have something akin to
"prerequisiteness" from course A to B and B to A. Now we see easily that if the
distance from A to B is shorter than from B to A then course A must be taken
before course B. Course A is more prerequisite to course B than the other way
around.

We have an order for the courses!

***Scheduling*** First to remove some unnecessary abstraction lets specify some
arbitrary limits to our curriculum.

- One year consists of 4 semesters.
- All courses must be possible to attend in 5 years (or 20 semesters).
- Each semester must have at maximum 15 credits of workload.

From these we can infer that the most credits that a curriculum can include is

20 semesters/curriculum * 15 credits/semester = 300 credits/curriculum.

If we have more than 300 credits worth of courses we must remove some from the
curriculum. The answer to "which courses" question is still missing. But one
option would be to filter out all the "latest" courses that appear in the
ordered set of courses such that we have less than 300 credits left.

This leads us to the last but not least of our ideas. We have a filtered set of
ordered courses and we have to schedule them according to our limits. One way
of solving this is with predicate logic and SMT solvers. For example our limits
can be reformatted a bit further to following statements.

- Course can have an integer value between 1 and 20 (semesters).
- Sum of scheduled credits in one semester must be less than 15.
- Course A must have a value less than course B.

Which can later be translated to solver code. SMT solvers are very useful in
practice because of two things; They provide a way to check if a solution
exists that meets our restrictions AND if such a solution exists they provide
the solution! (In case of many possible solutions we get just the first, but
that is still pretty useful!) This means that we can check if a schedule is
possible to construct with our restrictions and if so we get a schedule
constructed for us. Voilà!

# Summary

The combination of our proposed solutions can help in solving some problems that the current
meeting oriented process suffers. Our proposal removes the interpretation problem
with a restricted language, it helps with some of the tedious and error prone
work that goes into curriculum design, and it does speed up the process by
automating ordering and scheduling.

And on top of this it provides a formal, repeatable and understandable way of
constructing a curriculum that meets some common criterion.

1. All courses are ***possible to attend*** in a given number of years and
   semesters.
2. Each of the semesters have ***appropriate load*** in terms of credits/hours.
3. Each course shall ***build on top of previous knowledge***.


## Past work

[STOPS](https://dl.acm.org/doi/pdf/10.1145/2674683.2674689)

## Method

### Constructing a distance graph ([distance.rkt](src/distance.rkt))

#### Distance function

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
 ancestor (LCA) and tail lengths are added (see code below). Weight each distance
 by the average of depths of both nodes.

 ```scheme
(define (distance-nodes o n1 n2)
  (let ((p1 (path-from-root o n1))
        (p2 (path-from-root o n2)))
    (define (common l1 l2)
      (filter (lambda (x) (member x l1))
              l2))
    (* (- (+ (length p1) (length p2))
          (* 2 (length (common p1 p2))))
       (/ (* (length p1) (length p2)) 2))))
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

 5. And finally multiply the result with the product of both courses credits
multiplied together.

```
(* weight1 weight2 (average-closest-neighbour-distance t outs pres)))))
```

This distance function has some nice properties:

- Distance from course to itself is `0`. Minimum length from each node to self is 0.

#### Map to all courses

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

#### Filter

`H(u, th) -> ũ` is a filter removing any edges from source graph (u) below the
threshold value (th) and remove any cycles (out of two "bidirectional" edges
between two nodes remove the larger distance).

```scheme
(define (H u th)
  (define f (filter (lambda (x) (< (caddr x) th)) u))
  ;; Find the smaller of bidirectional edges between same nodes.
  ;; TODO: Does not work when weights are equal.
  (define (find-smaller-if-exists p l)
    (define r (filter (lambda (x) (and (eq? (car x) (cadr p))
                                        (eq? (cadr x) (car p))))
                      f))
    (if (> (length r) 0) ;; counterpart exists
        (if (< (caddr p) (caddr (car r)))
            p
            (car r))
        p))
  (remove-duplicates (for*/list ((p f))
                       (find-smaller-if-exists p f))))
```

Before the filter the graph looks like this ("inf"-edges removed).
![images/graph-no-filter.svg](images/graph-no-filter.svg)

Then we apply the threshold filter (th = 5, experimental).
![images/graph-th-filter.svg](images/graph-th-filter.svg)

And finally we remove bidirectionals.
![images/graph-th-filter-and-acyclic.svg](images/graph-th-filter-and-acyclic.svg)

From this we can make clear inference on what courses are prerequsite to other
and we can proceed to schedule the courses.

### Schedule ([smt.rkt](src/smt.rkt) and [dot.rkt](src/dot.rkt))

`M(ũ, s) -> m` creates a model curriculum (m) based on the filtered course
distance graph ũ. (A rought idea of this can be visualized with `make all`).

## Usage

```
# Install z3 theorem solver.

apt install z3

# Install racket.

add-apt-repository ppa:plt/racket
apt-get install racket

# Run scripts.
make
```

