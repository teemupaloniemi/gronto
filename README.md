# What?

Gronto is short for `GRaphs and ONTology to Opetussuunnitelma`. Gronto aims to
bring the [ACM Compute Classification System's](https://dl.acm.org/ccs)
benefits to curriculum design. We also touch some topics from [graph
theory](https://en.wikipedia.org/wiki/Graph_theory), and [satisfiability
solvers](https://en.wikipedia.org/wiki/Satisfiability_modulo_theories).

# Formally

- ${C}$ denotes all courses.
- ${N}$ denotes all ontology nodes.
- ${W}$ denotes integer weights [1,2,3,4,5,6].
- ${Cred} : {C} \to \mathbb{N}$ maps a course to credits.
- ${P} : {C} \to ({N}\times{W})^\ast$ maps a course to a list of weighted prerequisites.
- ${O} : {C} \to ({N}\times{W})^\ast$ maps a course to a list of weighted outcomes.
- ${f} : {({N}\times{W})}^2 \to ({N} \times {N} \times \mathbb{Q})$ gives the distance between two weighted ontology nodes.
- ${G} : ({N}\times{W})^\ast \times ({N}\times{W})^\ast \to \mathbb{Q}$ gives the distance between two lists of weighted ontology nodes.
  - ${G}$ is implemented as the average of ${f}$ mapped to the the cartesian product of ${O}(c1)$ and ${P}(c2)$ and filtered by the minimum for each pair where the first element is element of ${O}(c1)$.
- ${D} : {C} \times {C} \to ({C} \times {C} \times \mathbb{Q})$ gives the distance between two courses.
  - Here ${D}(c1, c2) = (c1, c2, {Cred}(c1) * {Cred}(c2) * {G}({O}(c1), {P}(c2)))$ for $c1,c2 : {C}$.

# Discussion

1. Yes, but why?

   - Attending and passing a course without prerequisite knowledge requires
     more effort than needed. This means students waste time and won't learn
     things.

   - Teaching and planning a course for students without prerequisite knowledge
     is near impossible. Most subjects don't have "the one way" to teach
     because incoming students vary.

   - Some sort of prerequisite and outcome documentation is performed
     currently, but it relies on free text fields and varying language. This
     means that everyone uses different phrasing and level of detail which
     easily leads to misunderstanding.

   - Often prerequisites are documented as course codes (TIEP112 has a
     prerequisites TIEP111) or course titles (Programming 1 is prerequisite for
     Programming 2). It is really hard to know what is expected from the
     incoming student.

   - Curriculum module building and scheduling relies on these
     "course-connections" rather than "knowledge-connetions". Paths like
     "Programming 1 -> Programming 2 -> Programming 3" might seem obvious. But
     it is not obvious how much and what knowledge the student has after those
     courses.

   - Not all courses are named with an ordinal attached to them. In which order
     should a student take courses like "signal processing, computer vision,
     and machine learning"?

   - When this happens systematically accross the whole curriculum (downstream
     effect is large) graduation times get longer, curriculum outcomes are not
     reached, and graduates are much worse equipped to become researchers or
     professionals.

   - An organization easily forgets what, how and why it makes decicions.
     Curriculum planning is a massive hurdle each 4 years, even when not that
     much has changed. New people are responsible for planning the curriculum
     and they start everything from scratch. No-one remembers what was done
     last time or 10 years ago. Making the process a bit more automatic and
     invariant to people would help.

2. Is the problem social or technical?

   - **Social**

     - Course prerequisites are based on personal knowledge that might be
       outdated or misguided. "I know what person A and B teach in their
       course." --- person C.

     - The documentation is currently hard to maintain. Teachers and staff
       don't believe that documenting (and scheduling) helps or they just don't
       want to do extra work. "The course topics evolve each year as new
       discoveries are made and research goes forward." - Teacher C

     - Techers want to hold on their freedom to teach and don't want to be
       monitored.

   - **Technical**

     - Current information systems (SISU, TIM, MOODLE, etc.) don't have a good
       data model for a course --- free text is encouraged.

     - Extracting some information is not made easy. Try to come up with a list
       of courses and their contents, of the current curriculum.

     - Modeling the example curriculum (which most students end up following)
       is done by hand and it is error prone.

3. Is the solution usable more than once?

   - Computing the prerequisite courses is iterable. This feels stable, I have
     no proof.

   - Scheduling depends on all courses. This might be unstable!

4. Test other possible distance metrics?

   - Current is the min-mean-distance weighted by the course credits.

5. Why particular ontology?

   - What else could be used?

      - [2020 Mathematics Subject Classification System](https://mathscinet.ams.org/mathscinet/msc/msc2020.html)

   - What properties should these have for them to work?

      - A way to determine distance between any two items.

      - It must span the space of all courses. Expressiveness of the ontology.
        We must be able to embed all courses to the ontology.

      - [Graph properties](https://en.wikipedia.org/wiki/Graph_property)?

6. How to evaluate and validate the effectiveness of the system?

   - **Short term**

      - Demonstration + Questionnaire. (Tells end user opinion.)

      - Speed. (Must show we do same thing as before but faster.)

      - By analogy or reference. (Must show someone else has done same with good results.)

   - **Long term**

      - Quality of graduating students, grades, jobs, publications, etc.

7. Why did we choose this method/these methods (racket, smt, graphs, ontology, etc) ?

   - Well first we have two problems.

       - Assign prerequisites.

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
                 ACM ontology is a graph.

       - Schedule courses based on prerequisites (and semester limits).

           - SMT (This was the first thing we tried. Could we show that this is
             the best? or argue that it is Good enough?

               - There exists lots of literature on University Course
                 Scheduling Problem or UCSP and the likes. From the two class
                 of solutions, exact or heuristic, we prefer exact. USCP is a
                 combinatorial problem and we just want something that performs
                 better than brute force search.

               - We could push for even better performace with methods described
                 in [Hossain et.al.](https://pure.ulster.ac.uk/ws/portalfiles/portal/76764440/accepted_version_Optimization_of_University_Course_Scheduling_Problem_NSiddique.pdf)
                 or [Kyngäs](https://www.utupub.fi/bitstream/handle/10024/72127/D140%20doria.pdf?sequence=1&isAllowed=y)
                 or [Xiang et.al.](https://doi.org/10.1016/j.eswa.2024.123383),
                 but SMT has many different implementations, is simple, well
                 maintained, understandable and it generalizes well to this
                 class of problems.

8. What end user gets?

   - Curriculum development does not have to start from scrach each year.

   - A scheduled curricula. Fast. What we mean is that the system can be
     incorporated in day-to-day operations or the organization.

       - Do we have dependency cycles in the current curriculum?

       - Is there a course that is important but does not have enough
         prerequisites. Could someone teach them? Should we hire someone? What
         kind of skills we need?

       - Is the same information taught somewhere twice? Can we combine
         courses, or introduce people with similar courses to each other?

       - What is the lightest curriculum that can build? What is the heaviest?

   - This also gives more tools for student counceling. For example we can
     give a lower bound in how many semesters the student can graduate.

       - No there is no way you can graduate in a year, if you take one course
         per semester!.

       - If you are willing to work, you can graduate before next christmas if
         you take these courses in this order.

       - Desining model curricula for different students. Some are working and
         can do only 5 credits per semester. How should they proceed?

       - Someone has to graduate fast, does not matter what courses to take. No
         problem, check if curricula with a 6 month timelimit is possible!

9. What end used has to provide?

   - Limits for the solver. (How many years, what work load, etc.)

   - Individual courses embedded into the ontology. (A questionnaire part of
     this masters thesis.)

   - An ontology. As an adjacency matrix/ adjacency lists (ACM included).

10. Compare to other solutions?
   - [STOPS – A Graph-Based Study Planning and Curriculum Development Tool](https://dl.acm.org/doi/pdf/10.1145/2674683.2674689)
       - [Educational Technologies for Supporting Self-Regulated Learning in Online Learning Environments](https://aaltodoc.aalto.fi/server/api/core/bitstreams/9a88834a-affe-4a5f-92fd-6b2340c6336b/content)

11. What remains for even better world?

12. End user experiances resulting from the use of this system?

13. What about maintenance? How long does this system survive in an
    organization? Does anyone actually care about this? Why should someone
    care? Can this be revived fast if someone care? Does the technology go
    obsolete? Will the ideas become obsolete? Will the system be adaptable
    to other universities?

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



