SOURCES="src/utils.ss"\
	"src/parse-tree.ss"\
	"src/svg-color.ss"\
	"src/sys.ss"\
	"src/compile.ss"

RACO=/Applications/Racket-v6.5/bin/raco 

compile:
	${RACO} make -v src/main.ss

test:
	${RACO} test ${SOURCES}
clean:
	find . -name '*.zo' -exec rm -f {} \;
	find . -name '*.dep' -exec rm -f {} \;
