.PHONY: tmp-folder compile prerequisites scheduler clean

all: tmp-folder compile

tmp-folder:
	mkdir -p tmp

compile:
	raco make -v src/*.rkt

clean:
	rm -rf tmp src/compiled src/utils/compiled
