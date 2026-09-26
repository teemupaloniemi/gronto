#!/usr/bin/bash

## sha256 of the dot files
if [ -f tmp/prerequisites.dot ]; then
  prerequisites_sha256=$(sha256sum tmp/prerequisites.dot)
else
  prerequisites_sha256=0
fi

if [ -f tmp/schedule.dot ]; then
  schedule_sha256=$(sha256sum tmp/schedule.dot)
else
  schedule_sha256=0
fi

## Compile
CORES=$(nproc)/2 make

# Iteratively increate the threshold until prerequisite graph is not empty.
i=1
while [ $i -le 1024 ]
do
  echo "filter threshold $i..."
  # Compute prerequisites
  # Params:
  #   [in]  "data/input.json"       courses path
  #   [out] "tmp/prerequisites.dot" result dot graph path
  #   [out] "tmp/output.json"       amended version of input courses
  #   [in]  "i"                    threshold for prerequisiteness (experimental)
  racket ./src/prerequisites.rkt data/input.json tmp/prerequisites.dot tmp/output.json $i
  if [ $? -eq 0 ]
  then
    echo "non-empty prerequisite graph found"
    break
  fi
  i=$((i*2))
done

if [ $? -eq 0 ]
then
  # Schedule the courses (if possible*)
  # Params:
  #   [in]  "tmp/output.json"       courses path
  #   [out] "tmp/schedule.dot"      result dot graph path
  #   [in]  "1"                     years in the curriculum, in which the course must be fit it
  #   [in]  "4"                     semesters per year
  #   [in]  "5"                     minimum credits per semeter
  #   [in]  "10"                    maximum credits per semeter
  racket ./src/scheduler.rkt tmp/output.json tmp/schedule.dot 1 4 0 5

  # Visualize, if files have changed
  if [ "$prerequisites_sha256" != "$(sha256sum tmp/prerequisites.dot)" ]; then
    xdot tmp/prerequisites.dot &
  fi

  if [ "$schedule_sha256" != "$(sha256sum tmp/schedule.dot)" ]; then
    xdot tmp/schedule.dot &
  fi
else
  echo "prerequisite graph was empty even after search and therefore schedule could not be computed!"
fi
