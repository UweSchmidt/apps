PL	= args-parser
PLrex	= (args-parser)

all	:
	$(foreach i,$(PL), ( cd $i && cabal configure && cabal build && cabal install && cabal sdist; ); )
	ghc-pkg list

reinstall:
	$(foreach i,$(PL), ( cd $i && cabal install; ); )
	ghc-pkg list

haddock	:
	$(foreach i,$(PL), ( cd $i && cabal haddock ); )

clean	:
	$(foreach i,$(PL), ( cd $i && cabal clean; ); )

unregister	:
	ghc-pkg list --simple-output | xargs --max-args=1 echo | egrep '$(PLrex)' | xargs --max-args=1 ghc-pkg --force unregister
	ghc-pkg list
