```bash
# Gronto is a simple university curriculum constructor. The schedule is
# constructed based on course definitions, in which prerequisites and outcomes
# are define from ACM CCS ontology. See [intput.json](data/input.json) for an
# example.

## Prerequisites

# Make and xdot
sudo apt-get install build-essential xdot

# Racket
sudo add-apt-repository ppa:plt/racket -y
sudo apt-get install racket -y

# Racket Generic Graph Library
raco pkg install graph

# Solver library
raco pkg install rosette

## Compiling
make

## Running

# Compute prerequisites
#
# Params:
#   [in]  "data/input.json"       individual courses in questionnaire format
#   [out] "tmp/prerequisites.dot" result dot graph
#   [out] "tmp/output.json"       amended version of input courses
#   [in]  "30"                    threshold for prerequisiteness
#
racket ./src/prerequisites.rkt data/input.json tmp/prerequisites.dot tmp/output.json 30

# Visualize
xdot tmp/prerequisites.dot

# Schedule the courses (if possible*)
#
# Params:
#   [in]  "tmp/output.json"       individual courses in questionnaire format
#   [out] "tmp/schedule.dot"      result dot graph
#   [in]  "2"                     years in the curriculum
#   [in]  "4"                     semesters per year
#   [in]  "5"                     minimum credits per semeter
#   [in]  "10"                    maximum credits per semeter
#
racket ./src/scheduler.rkt tmp/output.json tmp/schedule.dot 2 4 5 10

# Visualize
xdot tmp/schedule.dot
```
