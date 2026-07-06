.PHONY: tmp-folder compile prerequisites scheduler clean

CORES ?= 1

all: tmp-folder compile

tmp-folder:
	mkdir -p tmp

compile:
	raco make -j $(CORES) -v src/*.rkt

clean:
	rm -rf tmp src/compiled src/utils/compiled
