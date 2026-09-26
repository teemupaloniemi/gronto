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


# Initial computation of prerequisite graph with large threshold
# (practically this means no filtering).
i=32
echo "filter threshold set to $i..."
racket ./src/prerequisites.rkt data/input.json tmp/prerequisites.dot tmp/output.json $i
racket ./src/scheduler.rkt tmp/output.json tmp/schedule.dot 2 4 0 20
e=$?

if [ $e -ne 0 ]
then
  # Approximate search with binary search.
  echo "--- binary search ---"
  while [ $e -ne 0 ]
  do
    # Exit code of 1 means that the prerequisite graph has cycles.
    i=$((i/2))
    echo "filter threshold set to $i..."
    while [ $e -ne 0 ]
    do
      racket ./src/prerequisites.rkt data/input.json tmp/prerequisites.dot tmp/output.json $i
      e=$?
      if [ $e -eq 1 ]
      then
        i=$((i*3))
      fi
    done
    racket ./src/scheduler.rkt tmp/output.json tmp/schedule.dot 2 4 0 20
    e=$?
  done

  echo "valid schedule found"

  # Detailed linear search.
  echo "--- detailed search ---"
  a=$i
  t=$((i/20))
  e=0
  while [ $e -eq 0 ]
  do
    i=$((i+t))
    echo "increasing filter threshold to $i..."
    racket ./src/prerequisites.rkt data/input.json tmp/prerequisites.dot tmp/output.json $i
    racket ./src/scheduler.rkt tmp/output.json tmp/schedule.dot 2 4 0 20
    e=$?
  done
  echo "in-valid schedule found"
  i=$((i-t))
fi

racket ./src/prerequisites.rkt data/input.json tmp/prerequisites.dot tmp/output.json $i
racket ./src/scheduler.rkt tmp/output.json tmp/schedule.dot 2 4 0 20

# Visualize, if files have changed
if [ "$prerequisites_sha256" != "$(sha256sum tmp/prerequisites.dot)" ]; then
  xdot tmp/prerequisites.dot &
fi

if [ "$schedule_sha256" != "$(sha256sum tmp/schedule.dot)" ]; then
  xdot tmp/schedule.dot &
fi
