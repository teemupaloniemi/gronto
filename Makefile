.PHONY: tmp-folder compile prerequisites scheduler clean

COMPILER=raco exe

all: tmp-folder compile

tmp-folder:
	mkdir -p tmp

compile: prerequisites scheduler

scheduler: src/scheduler.rkt
	$(COMPILER) -o tmp/scheduler src/scheduler.rkt

prerequisites: src/prerequisites.rkt
	$(COMPILER) -o tmp/prerequisites src/prerequisites.rkt

clean:
	rm -rf tmp
