SOURCES="src/utils.ss"\
	"src/parse-tree.ss"\
	"src/svg-color.ss"\
	"src/sys.ss"\
	"src/compile.ss"\
	"src/rackscad.ss"\
	"src/solids.ss"

RACO=/Applications/Racket/bin/raco 

compile:
	${RACO} make -v main.rkt

test:
	${RACO} test ${SOURCES}
clean:
	find . -name '*.zo' -exec rm -f {} \;
	find . -name '*.dep' -exec rm -f {} \;
package:
	make clean
	make
	cd ..; ${RACO} pkg create --dest rackscad rackscad; cd rackscad

remove:
	${RACO} pkg remove rackscad

install:
	${RACO} pkg install rackscad.zip

reinstall: remove install 
