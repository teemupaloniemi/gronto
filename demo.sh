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
i=128

# Iteratively check exit codes and adjust the threshold until a valid schedule
# is found.
while [ 0 -eq 0 ]
do
  # Exit code of 1 means that the prerequisite graph has cycles. First
  # instinct is to decrease the filter threshold. But if that leads to an
  # empty prerequisite graph, the threshold must be increased slightly.
  # Resulting threshold should be in the range "old_th/2" <= th <= "old_th".
  k=$i
  l=$((k/2))
  i=$((i/2))
  n=2
  echo "lowering filter threshold to $l <= $i <= $k..."
  while [ $i -le $k ]
  do
    # Compute prerequisites
    # Params:
    #   [in]  "data/input.json"       courses path
    #   [out] "tmp/prerequisites.dot" result dot graph path
    #   [out] "tmp/output.json"       amended version of input courses
    #   [in]  "i"                    threshold for prerequisiteness (experimental)
    racket ./src/prerequisites.rkt data/input.json tmp/prerequisites.dot tmp/output.json $i

    e=$?

    if [ $e -eq 0 ]
    then
      break
    elif [ $e -eq 1 ]
    then
      n=$((n*2))
      i=$((i+(k/n)))
      echo "increasing filter threshold to $l <= $i <= $k..."
    fi
  done

  # Schedule the courses (if possible*)
  # Params:
  #   [in]  "tmp/output.json"       courses path
  #   [out] "tmp/schedule.dot"      result dot graph path
  #   [in]  "2"                     years in the curriculum, in which the course must be fit it
  #   [in]  "4"                     semesters per year
  #   [in]  "5"                     minimum credits per semeter
  #   [in]  "15"                    maximum credits per semeter
  racket ./src/scheduler.rkt tmp/output.json tmp/schedule.dot 2 4 0 15

  e=$?

  # Exit code of 0 means that a valid schedule was found.
  if [ $e -eq 0 ]
  then
    echo "valid schedule found"
    break
  fi
done

# Visualize, if files have changed
if [ "$prerequisites_sha256" != "$(sha256sum tmp/prerequisites.dot)" ]; then
  xdot tmp/prerequisites.dot &
fi

if [ "$schedule_sha256" != "$(sha256sum tmp/schedule.dot)" ]; then
  xdot tmp/schedule.dot &
fi
