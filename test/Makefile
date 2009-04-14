SRC	= $(wildcard ../src/*/*.hs)

prog	= photo2
prog2	= photoEdit

progs	= $(prog) $(prog2)

all	: $(progs)

$(prog)	: A.hs $(SRC)
	ghc --make -Wall -O2 -i../src -o $@ A.hs

$(prog2): Main.hs $(SRC)
	ghc --make -Wall -O2 -i../src -o $@ Main.hs

install		: $(progs)
		for i in $(progs) ;\
		do \
			[ ! -x "~/bin/$$i" ] || mv -f "~/bin/$$i" "~/bin/$$i.bak" ; \
			cp $$i ~/bin ; \
		done

clean	:
	rm -f $(SRC:.hs=.hi)
	rm -f $(SRC:.hs=.o)
	rm -f A.hi A.o Main.hi Main.o $(progs)

