prog	= find-grep-sed

all	: $(prog)

$(prog)	: Find.hs
	ghc -O2 -Wall --make -o $@ $<

$(prog)	: FindGrepSed.hs

install	: all
	[ ! -f ~/bin/$(prog) ] || mv -f ~/bin/$(prog) ~/bin/$(prog)~
	cp -f $(prog) ~/bin

test	: $(prog)
	$(prog) hunitTest

