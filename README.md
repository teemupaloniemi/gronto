# What?

Gronto is short for `GRaphs and ONTology to Opetussuunnitelma`. Gronto aims to
bring the [ACM Compute Classification System's](https://dl.acm.org/ccs)
benefits to curriculum design. We also touch some topics from [graph
theory](https://en.wikipedia.org/wiki/Graph_theory), and [satisfiability
solvers](https://en.wikipedia.org/wiki/Satisfiability_modulo_theories).

# TODO

- Yes, but why?
- Is the problem social or technical?
   - Both, but we aim to work on the technical problems.

- Collect more data?
   - Is it iterable or one-shot? Stability wrt data?
      - Computing the prerequisite courses is iterable. This feels stable.
      - Scheduling depends on all courses. This might be unstable!

- Test other possible distance metrics?

- Test other possible ontologies?
   - What properties should these things have for them to work?
      - A way to determine distance between any two items.
      - It must span the space of all courses. Expressiveness of the ontology.
        We must be able to embed all courses to the ontology.

- How to evaluate the effectiveness of the system?
- Why did we choose this method/these methods?
   - racket, smt, graphs, ontology, etc.

- What end user gets?
   - A scheduled curricula. Fast.

- What end used has to provide?
   - An ontology.
   - Individual courses embedded into the ontology.

- Compare to other solutions?
   - STOPS

- What remains for even better world?
- End user experiances resulting from the use of this system?

- What about maintenance?
   - We don't only give implementation, we also try to explain the higher level
     ideas and reasoning for building it.

- How long does this system survive in an organization?
   - When adapting to this system the organization receives a documented
     curricula (opinto-opas) but also -- maybe more importantly -- the creation
     process is synthetically documented. People often leave the organization
     but their ideas and decicions are captured by the system.

## Running

`make`
