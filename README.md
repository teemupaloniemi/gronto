# What?

Gronto is short for `GRaphs and ONTology to Opetussuunnitelma`. Gronto aims to
bring the [ACM Compute Classification System's](https://dl.acm.org/ccs)
benefits to curriculum design. We also touch some topics from [graph
theory](https://en.wikipedia.org/wiki/Graph_theory), and [satisfiability
solvers](https://en.wikipedia.org/wiki/Satisfiability_modulo_theories).

# Prerequisites

- make
- racket / raco
  - Racket Generic Graph Library (`raco pkg install graph`)
  - Solver library (`raco pkg install rosette`)
- xdot
- sha256sum

# Compile

We must first compile the code (make), then compute
all-pair-distances of our ontology network (make distance), and finally compile again to link
the newly computed part to main program (make compile).

```
make
make distance
make compile
```

Now we have a compiled version of the system.

# Running

`demo.sh` will start a new xdot session and track the state of
`data/input.json`. If it is change then `make distance solve` is executed.

```
./demo.sh
```

Now you can open the file `data/input.json`, in your favourite text editor,
which contains the course data and start manipulating it. You can also change
the restrictions for the solver in `src/dot.rkt` to change number of years,
semsters, semester limits etc.

# TODO

1. Yes, but why?
   - Course prerequisite assignment seems to be the biggest unsolved problem in
     curriculum design.
       - Here we could show the space of curriculum desing problems and show
         how this relates to them.
   - Scheduling just helps in practice, it has been solved million times before
     so we don't ellaborate on it now.

2. Is the problem social or technical?
   - Both, but we aim to work on the technical problems.

3. Collect more data?
   - Is it iterable or one-shot? Stability wrt data?
      - Computing the prerequisite courses is iterable. This feels stable.
      - Scheduling depends on all courses. This might be unstable!

4. Test other possible distance metrics?

5. Test other possible ontologies?
   - [2020 Mathematics Subject Classification System](https://mathscinet.ams.org/mathscinet/msc/msc2020.html)
   - What properties should thes have for them to work?
      - General
          - A way to determine distance between any two items.
          - It must span the space of all courses. Expressiveness of the ontology.
            We must be able to embed all courses to the ontology.
      - [Graph properties](https://en.wikipedia.org/wiki/Graph_property)?

6. How to evaluate and validate the effectiveness of the system?
   1. Short term
       - We could show that the courses in computed curricula are in "better"
         arrangement than in the hand made curricula. But we define the metric
         so this is a bit unfair.
   2. Long term
       - Quality of graduating students, grades, jobs, publications, etc.

7. Why did we choose this method/these methods (racket, smt, graphs, ontology, etc) ?
   - Well first we have two problems.
       - Assign prerequisite courses to courses.
           - Ontology
               - We need some way to tell how "prerequisite" a course is to
                 other courses. In other terms we need some "similarity"
                 measurement of any two courses, and this is a question of
                 semantics. We could use propabilistic solutions i.e. sentence
                 embeddings, but we strive for deterministic methods. Ontology
                 gives semantics and as long as we comply with our requirements (Q
                 in choosing ontology we have a deterministic way of measuring
                 similarity.
               - Ok, but why is probabilistic bad? Can we order propabilistic vs
                 deterministic?
           - Graphs
               - Because they provide a distance between items inctrinsically.
                 Graphs because they are nice to visualize. Graphs because the
                 ACM ontology is a tree.

       - Schedule courses based on prerequisites (and semester limits).
           - SMT
               - There exists lots of literature on University Course
                 Scheduling Problem or UCSP and the likes. From the two class
                 of solutions, exact or heuristic, we prefer exact. USCP is a
                 combinatorial problem and we just want something neat that
                 performs better than brute force search. We could push for
                 even better performace with methods described in [Hossain et.al.](https://pure.ulster.ac.uk/ws/portalfiles/portal/76764440/accepted_version_Optimization_of_University_Course_Scheduling_Problem_NSiddique.pdf) or [Kyngäs](https://www.utupub.fi/bitstream/handle/10024/72127/D140%20doria.pdf?sequence=1&isAllowed=y)
                 or [Xiang et.al.](https://doi.org/10.1016/j.eswa.2024.123383),
                 but SMT has many different implementations, is simple, well
                 maintained, understandable and it generalizes well to this
                 class of problems.

       - Racket, I guess I just wanted to learn racket better?

8. What end user gets?
   - A scheduled curricula. Fast. What we mean is that the system can be
     incorporated in day-to-day operations of the curriculum design process.
       - For example the people designing curriculum can run the system
         iteratively for each course and see how the curriculum evolves. They
         can play around with the restrictions in the solver i.e. making the
         curriculum "lighter" by reducing the number of credits for each
         semester. This way they can end up answering many interesting
         questions that were out of reach before because of time and resource
         constraints.
           - "What is the lightest curriculum that can build? What is the heaviest?"
           - "Can a student graduate in 2-years if they want?"
       - This also gives more tools for student counceling. For example we can
         give a lower bound in how many semesters the student can graduate.
           - "No there is no way you can graduate in a year, if you take one
             course per semester!".
           - "If you are willing to work, you can graduate before next
             christmas if you take these courses in this order" - add system
             output here.

9. What end used has to provide?
   - An ontology. As an adjacency matrix/ adjacency lists (ACM included).
   - Individual courses embedded into the ontology.

10. Compare to other solutions?
   - [STOPS – A Graph-Based Study Planning and Curriculum Development Tool](https://dl.acm.org/doi/pdf/10.1145/2674683.2674689)
       - [Educational Technologies for Supporting Self-Regulated Learning in Online Learning Environments](https://aaltodoc.aalto.fi/server/api/core/bitstreams/9a88834a-affe-4a5f-92fd-6b2340c6336b/content)

11. What remains for even better world?
12.  End user experiances resulting from the use of this system?

13.  What about maintenance?
   - We don't only give implementation, we also try to explain the higher level
     ideas and reasoning for building it.

14. How long does this system survive in an organization?
   - When adapting to this system the organization receives a documented
     curricula (opinto-opas) but also -- maybe more importantly -- the creation
     process is synthetically documented. People often leave the organization
     but their ideas and decicions are captured by the system.

