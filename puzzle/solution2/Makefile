N=10

maxSteps	= 0
queueSize	= 500000
trc             = False
ghcOpts         = +RTS -K20M -RTS

all:
	cabal configure && cabal build

name:
	@echo "Under Cover Agent 42"

run:
	@./dist/build/squares/squares $(ghcOpts) $(maxSteps) $(queueSize) $(trc)

test:
	runhaskell Gen.hs classic $(N) | nice ./dist/build/squares/squares $(ghcOpts) $(maxSteps) $(queueSize) $(trc)

test10:
	$(MAKE) test N=10 trc=True

test50:
	$(MAKE) test N=50

test56:
	$(MAKE) test N=56

overflow:
	$(MAKE) test N=56 queueSize=0

## generate problem

type 		= classic
generator 	= runhaskell Gen.hs $(type) $(N)
gen             = $(MAKE) --quiet problem

problem		: ; @$(generator)

classic20 	: ; @$(gen) type=classic N=20
classic50 	: ; @$(gen) type=classic N=50
classic56 	: ; @$(gen) type=classic N=56
classic60 	: ; @$(gen) type=classic N=60
classic100 	: ; @$(gen) type=classic N=100
classic200 	: ; @$(gen) type=classic N=200

classic2-50 	: ; @$(gen) type=classic2 N=50
classic2-56 	: ; @$(gen) type=classic2 N=56
classic2-60 	: ; @$(gen) type=classic2 N=60
classic2-100 	: ; @$(gen) type=classic2 N=100
classic2-200 	: ; @$(gen) type=classic2 N=200

imperfect23	: ; @$(gen) type=imperfect23 N=0
imperfect23Hole	: ; @$(gen) type=imperfect23 N=1
imperfect23All	: ; @$(gen) type=imperfect23 N=2

perfect112	: ; @$(gen) type=perfect112 N=0
perfect112Plus1 : ; @$(gen) type=perfect112 N=1
perfect112All	: ; @$(gen) type=perfect112 N=2

perfect110	: ; @$(gen) type=perfect110 N=0
perfect110Plus1 : ; @$(gen) type=perfect110 N=1
perfect110All	: ; @$(gen) type=perfect110 N=2

perfect308	: ; @$(gen) type=perfect308 N=0
perfect308Plus2 : ; @$(gen) type=perfect308 N=1
perfect308All	: ; @$(gen) type=perfect308 N=2
