PL	= args-parser find-grep-sed monad-error-rws-io fgs2
PLrex	= (args-parser|find-grep-sed|monad-error-rws-io fgs2)

all	:
	$(foreach i,$(PL), ( cd $i && cabal install && cabal sdist; ); )
	$(MAKE) photo2
	ghc-pkg list

photo2	:
	( cd photo2 && cabal install --flags="photoEdit photo2" )

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

.PHONY	: all reinstall haddock clean unregister photo2

