.PHONY: tmp-folder compile distance solve clean

all: tmp-folder compile

tmp-folder:
	mkdir -p tmp
compile:
	raco exe -o tmp/distance src/distance.rkt
	raco exe -o tmp/dot src/dot.rkt
solve:
	./tmp/dot
distance:
	./tmp/distance
visualize:
	xdot tmp/distance.dot &
	xdot tmp/courses.dot &
clean:
	rm -rf tmp
