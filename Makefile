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
