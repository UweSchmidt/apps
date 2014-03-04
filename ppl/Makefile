##
##help
##	this target

help	:
	@grep ^## Makefile | sed 's|##||'

##
##all
##	make pplc compiler

all	:
	cabal configure && cabal build && cabal install
	cp ./dist/build/pplc/pplc .

##
##ex
##	run all examples in examples dir
##

ex	:
	$(MAKE) -C examples all

profile	:
	$(MAKE) -C src profile

##
##dist
##	make all, ex, all pictures and tar archive

VERSION := $(shell grep '^version' ppl.cabal | sed 's|^version:[ ]*||')

dist	:
	cabal sdist
	cp ./dist/ppl-$(VERSION).tar.gz ppl.tar.gz


##
##clean
##	delete all generated files

clean	:
	cabal clean
	$(MAKE) -C src      clean
	$(MAKE) -C examples clean
	rm -rf $(wildcard dist/*)

##
##tgz
##	create ppl.tar.gz file
##

SRC	= $(wildcard src/Makefile src/Parser.* src/*.hs src/*/*.hs)
EXS	= $(wildcard examples/org/*.* examples/*.ppl examples/Makefile examples/java/ppl/[A-Z]*.java examples/c/*.h)

tgz	:
	[ -d dist/ppl ] || mkdir -p dist/ppl
	tar cf - $(SRC) $(EXS) Makefile README | ( cd dist/ppl ; tar xf - )
	[ -d dist/ppl/examples/gen ] || mkdir -p dist/ppl/examples/gen
	( cd dist ; tar cvzf ../ppl.tar.gz ppl )

.PHONY	: all ex profile dist clean tgz
