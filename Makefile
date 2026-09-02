b build:
	stack build

t test:
	stack test

bt build-test:
	stack test --no-run-tests

d docs:
	stack haddock

cl clean:
	stack clean --full

hackage: 
	rm -f dist-newstyle/*-docs.tar.gz
	rm -f dist-newstyle/sdist/*.tar.gz
	stack clean --full
	cabal build
	cabal sdist
	cabal v2-haddock --haddock-for-hackage --enable-doc