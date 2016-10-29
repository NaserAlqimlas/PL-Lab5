JSYDIR=../../../../../..
JSY=./reflab5.sh

CWD :=$(shell dirname $(realpath $(lastword $(MAKEFILE_LIST))))

TESTS=$(wildcard *.jsy)
ANSWERS=${TESTS:%.jsy=%.ans}

all: ${ANSWERS}
clean:
	rm -f ${ANSWERS}

%.ans: %.jsy
	cd ${JSYDIR}; \
	${JSY} ${CWD}/$< >${CWD}/$@
