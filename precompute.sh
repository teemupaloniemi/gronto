# compile the program without precomputed values
make

# compute the precomputed values
./tmp/prerequisites data/input.json tmp/prerequisites.dot tmp/output.json 15

# compile with the precomputed values
make
