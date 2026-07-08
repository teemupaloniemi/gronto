```bash
# Gronto is a curriculum constructor. It is composed of two parts. Computation
# of prerequisites and scheduling.

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
