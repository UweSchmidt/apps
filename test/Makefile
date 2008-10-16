SRC	= $(wildcard ../src/Photo2/*.hs)

all	: photo2

photo2	: A.hs $(SRC)
	ghc --make -Wall -O2 -i../src -o $@ A.hs

clean	:
	rm -f $(SRC:.hs=.hi)
	rm -f $(SRC:.hs=.o)
	rm -f A.hi A.o photo2

