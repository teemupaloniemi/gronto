# What?

Gronto is short for `GRaphs and ONTology to Opetussuunnitelma`. Gronto aims to
bring the [ACM Compute Classification System's](https://dl.acm.org/ccs)
benefits to curriculum design. We also touch some topics from [graph
theory](https://en.wikipedia.org/wiki/Graph_theory), and [satisfiability
solvers](https://en.wikipedia.org/wiki/Satisfiability_modulo_theories).

# TODO

1. Yes, but why?
2. Is the problem social or technical?
   - Both, but we aim to work on the technical problems.

3. Collect more data?
   - Is it iterable or one-shot? Stability wrt data?
      - Computing the prerequisite courses is iterable. This feels stable.
      - Scheduling depends on all courses. This might be unstable!

4. Test other possible distance metrics?

5. Test other possible ontologies?
   - [2020 Mathematics Subject Classification System](https://mathscinet.ams.org/mathscinet/msc/msc2020.html)
   - What properties should these things have for them to work?
      - A way to determine distance between any two items.
      - It must span the space of all courses. Expressiveness of the ontology.
        We must be able to embed all courses to the ontology.

6. How to evaluate and validate the effectiveness of the system?
   1. Short term
       - We could show that the courses in computed curricula are in "better"
         arrangement than in the hand made curricula. But we define the metric
         so this is a bit unfair.
   2. Long term
       - Quality of graduating students, grades, jobs, publications, etc.

7. Why did we choose this method/these methods (racket, smt, graphs, ontology, etc) ?
   - Well first we have two problems.
       1. Assign prerequisite courses to courses.
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

       2. Schedule courses based on prerequisites (and semester limits).
           - SMT
               - There exists lots of literature on University Course Scheduling
                 Problem or UCSP and the likes. From the two class of solutions,
                 exact or heuristic, we prefer exact. USCP is a combinatorial
                 problem and we just want something neat that performs better
                 than brute force search. We could push for even better
                 performace with i.e. optimization methods, but SMT has many
                 different implementations, is simple, well maintained,
                 understandable and it generalizes well to this class of
                 problems.

       - Racket, I guess I just wanted to learn racket better?

8. What end user gets?
   - A scheduled curricula. Fast.

9. What end used has to provide?
   - An ontology. As an adjacency matrix/ adjacency lists (ACM included).
   - Individual courses embedded into the ontology.

10. Compare to other solutions?
   - STOPS

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

## Running

`make`
