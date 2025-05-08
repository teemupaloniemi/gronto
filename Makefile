all: tmp-folder gen-smt solve-smt visualize

tmp-folder:
	mkdir -p tmp-files
gen-smt:
	racket src/smt.rkt > tmp-files/courses.smt
solve-smt:
	z3 -smt2 tmp-files/courses.smt > tmp-files/z3.txt
visualize:
	racket src/dot.rkt > tmp-files/courses.dot && xdot tmp-files/courses.dot

clean:
	rm -rf tmp-files
