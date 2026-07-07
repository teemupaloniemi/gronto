#!/usr/bin/bash

## sha256 of the dot files
prerequisites_sha256=$(sha256sum tmp/prerequisites.dot)
schedule_sha256=$(sha256sum tmp/schedule.dot)

## Compile
CORES=$(nproc)/2 make

# Compute prerequisites
# Params:
#   [in]  "data/input.json"       courses path
#   [out] "tmp/prerequisites.dot" result dot graph path
#   [out] "tmp/output.json"       amended version of input courses
#   [in]  "30"                    threshold for prerequisiteness (experimental)
racket ./src/prerequisites.rkt data/input.json tmp/prerequisites.dot tmp/output.json 1

# Schedule the courses (if possible*)
# Params:
#   [in]  "tmp/output.json"       courses path
#   [out] "tmp/schedule.dot"      result dot graph path
#   [in]  "1"                     years in the curriculum, in which the course must be fit it
#   [in]  "4"                     semesters per year
#   [in]  "5"                     minimum credits per semeter
#   [in]  "10"                    maximum credits per semeter
racket ./src/scheduler.rkt tmp/output.json tmp/schedule.dot 3 4 5 15

# Visualize, if files have changed
if [ "$prerequisites_sha256" != "$(sha256sum tmp/prerequisites.dot)" ]; then
  xdot tmp/prerequisites.dot &
fi

if [ "$schedule_sha256" != "$(sha256sum tmp/schedule.dot)" ]; then
  xdot tmp/schedule.dot &
fi
