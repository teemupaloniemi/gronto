```bash
# Gronto is a simple university curriculum constructor. The schedule is
# constructed based on course definitions, in which prerequisites and outcomes
# are define from ACM CCS ontology. See data/input.json for an example.

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

## Running

./demo.sh
```
