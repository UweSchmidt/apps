# $Id: Makefile,v 1.34 2006/01/08 14:20:13 uwe Exp $

HC		= ghc
HCFLAGS		=

GHCPROFILE	= -prof -auto-all
GHCEXTRA	=
GHCFLAGS	= -Wall  -fno-warn-unused-do-bind -O2 $(GHCEXTRA)

HCFLAGS		= $(GHCFLAGS)

##
##help
##	this target

help	:
	@grep ^## Makefile | sed 's|##||'

# SRCimg	= $(wildcard Matrix/*.hs)
SRCimg		= $(wildcard ImgFct/*.hs)

SRC	= $(wildcard PPL/*.hs) $(SRCimg) PPL/Parser.hs PPL.hs
OBJ	= $(SRC:.hs=.o)
IFS	= $(SRC:.hs=.hi)

PPLC	= ../pplc

##
##all
##	make pplc compiler, test prog for tree output
##	compile ppl-test programs
##	run some of the test programs
##	and generate java code

all	:
	$(MAKE) $(PPLC) GHCEXTRA="$(GHCEXTRA)"

##
##prof
##	make pplc compiler with profiling options

profile	:
	$(MAKE) all GHCEXTRA="$(GHCEXTRA) $(GHCPROFILE)"


##
##spaceProfile
##	make a run with samples for space statistics
##	pplc has to be generated with "make profile"

spaceProfile	:
	$(PPLC) +RTS -K2000000 -A10000000 -M200000000 -hC -RTS --exec example.ppl
	hp2ps pplc.hp
	kghostview pplc.ps &

##
##dist
##	make all and tar archive

dist	:
	$(MAKE) all GHCEXTRA="$(GHCEXTRA)"
	$(MAKE) Parser.info1

PPL/Parser.hs Parser.info	: Parser.y
	happy --info -a $< ; mv Parser.hs PPL/Parser.hs

%.info1	: %.info
	cat $< | cut -b 1-72 > $@

$(PPLC)	: $(SRC)
	$(HC) --make $(HCFLAGS) -o $(PPLC) PPL.hs

##
##clean
##	delete all generated files

clean	:
	rm -f $(OBJ) $(IFS) PPL/Parser.hs Parser.info*

.PHONY	: all profile dist spaceProfile clean

