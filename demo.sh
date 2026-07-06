## Compile
make

# Compute prerequisites
# Params:
#   [in]  "data/input.json"       individual courses in questionnaire format
#   [out] "tmp/prerequisites.dot" result dot graph
#   [out] "tmp/output.json"       amended version of input courses
#   [in]  "30"                    threshold for prerequisiteness (experimental)
racket ./src/prerequisites.rkt data/input.json tmp/prerequisites.dot tmp/output.json 30

# Schedule the courses (if possible*)
# Params:
#   [in]  "tmp/output.json"       individual courses in questionnaire format
#   [out] "tmp/schedule.dot"      result dot graph
#   [in]  "2"                     years in the curriculum
#   [in]  "4"                     semesters per year
#   [in]  "5"                     minimum credits per semeter
#   [in]  "10"                    maximum credits per semeter
racket ./src/scheduler.rkt tmp/output.json tmp/schedule.dot 2 4 5 10

# Visualize
xdot tmp/schedule.dot &
xdot tmp/prerequisites.dot &
