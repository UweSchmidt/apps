VERSION := $(shell grep '^version' ppl.cabal | sed 's|^version:[ ]*||')

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

##
##dist
##	make all, ex, all pictures and tar archive

dist	:
	$(MAKE) -C src/PPL sdist
	cabal sdist
	cp ./dist/ppl-$(VERSION).tar.gz ppl.tar.gz


##
##clean
##	delete all generated files

clean	:
	cabal clean
	$(MAKE) -C src/PPL  clean
	$(MAKE) -C examples clean
	rm -rf $(wildcard dist/*)

##
##version
##	set version # in pplc main prog

version	: src/PPL.hs

src/PPL.hs : ppl.cabal
	perl -i~ -p -e 's/version \d+\.\d+\.\d+\.\d+/version $(VERSION)/' $@

.PHONY	: all clean dist ex help version
