SRC	= $(wildcard ../src/*/*.hs)

prog	= photo2

all	: $(prog)

$(prog)	: A.hs $(SRC)
	ghc --make -Wall -O2 -i../src -o $@ A.hs

install		: $(prog)
		[ ! -x "~/bin/$(prog)" ] || mv -f "~/bin/$(prog)" "~/bin/$(prog).bak"
		cp $(prog) ~/bin

clean	:
	rm -f $(SRC:.hs=.hi)
	rm -f $(SRC:.hs=.o)
	rm -f A.hi A.o $(prog)

