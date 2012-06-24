N=10

all:
	cabal clean && cabal configure && cabal build

name:
	@echo "Under Cover Agent 007"

run:
	@./dist/build/squares/squares

test:
	runhaskell Gen.hs $(N) | ./dist/build/squares/squares
