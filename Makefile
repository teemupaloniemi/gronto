.PHONY: tmp-folder distance solve clean

all: tmp-folder distance solve visualize

tmp-folder:
	mkdir -p tmp
solve:
	racket src/dot.rkt
distance:
	racket src/distance.rkt > tmp/distance.dot
visualize:
	xdot tmp/distance.dot &
	xdot tmp/courses.dot &
clean:
	rm -rf tmp
