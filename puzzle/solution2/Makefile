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
	runhaskell Gen.hs $(N) | nice ./dist/build/squares/squares $(ghcOpts) $(maxSteps) $(queueSize) $(trc)

test10:
	$(MAKE) test N=10 trc=True

test50:
	$(MAKE) test N=50

test56:
	$(MAKE) test N=56

overflow:
	$(MAKE) test N=56 queueSize=0