.PHONY: tmp-folder compile distance solve clean

all: tmp-folder compile

tmp-folder:
	mkdir -p tmp
compile: compile-distance compile-dot
compile-dot: src/dot.rkt
	raco exe -o tmp/dot src/dot.rkt
compile-distance: src/distance.rkt
	raco exe -o tmp/distance src/distance.rkt
solve:
	./tmp/dot tmp/output.json 2 4 0 15
distance:
	./tmp/distance data/input.json tmp/distance.dot tmp/output.json
visualize:
	xdot tmp/distance.dot &
	xdot tmp/courses.dot &
clean:
	rm -rf tmp
