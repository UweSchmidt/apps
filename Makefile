prog	= ./find-grep-sed

all	: $(prog)

$(prog)	: Find.hs
	ghc -O2 -Wall --make -o $@ $<

$(prog)	: FindGrepSed.hs


clean	:
	rm -f *.hi *.o

distclean	: clean
	rm -f $(prog)

install	: all
	[ ! -f ~/bin/$(prog) ] || mv -f ~/bin/$(prog) ~/bin/$(prog)~
	cp -f $(prog) ~/bin

test	: $(prog)
	$(prog) hunitTest

