all: tmp-folder gen-smt solve-smt visualize

tmp-folder:
	mkdir -p tmp
gen-smt:
	racket src/smt.rkt > tmp/courses.smt
solve-smt:
	z3 -smt2 -st tmp/courses.smt > tmp/z3.txt
visualize:
	racket src/dot.rkt > tmp/courses.dot
	xdot tmp/courses.dot

clean:
	rm -rf tmp
