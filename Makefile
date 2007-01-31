
prog = arg-test

all	: $(prog)

$(prog)	: Test.hs ArgumentParser.hs
	ghc -Wall -O2 -o $(prog) --make Test.hs

clean	:
	rm -f *.hi *.o
