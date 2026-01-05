.PHONY: tmp-folder compile prerequisites scheduler clean

COMPILER=raco exe

all: tmp-folder precomputed compile

# If the precomputed.rkt file does not exist make one.
precomputed:
	if [ ! -f src/precomputed.rkt ]; \
	then \
	  printf "#lang racket\n\n(provide precomputed)\n(define precomputed #f)\n" > src/precomputed.rkt ; \
	fi;

tmp-folder:
	mkdir -p tmp

compile: prerequisites scheduler

scheduler: src/scheduler.rkt
	$(COMPILER) -o tmp/scheduler src/scheduler.rkt

prerequisites: src/prerequisites.rkt
	$(COMPILER) -o tmp/prerequisites src/prerequisites.rkt

clean:
	rm -rf tmp
