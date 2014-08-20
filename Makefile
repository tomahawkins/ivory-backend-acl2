.PHONY: all
all: test

.PHONY: test
test: Testcases.hs
	cabal install
	runhaskell -W Testcases.hs

.PHONY: clean
clean:
	cabal clean

