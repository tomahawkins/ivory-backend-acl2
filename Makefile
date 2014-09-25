.PHONY: all
.PRECIOUS: test.log
all: test.log

test.log: Testcases.hs src/Ivory/Opts/*.hs src/Ivory/Opts/Asserts/*.hs src/Ivory/Compile/*.hs src/Ivory/Compile/ACL2/*.hs
	cabal build && cabal install
	runhaskell -W Testcases.hs | tee test.log

.PHONY: clean
clean:
	cabal clean
	-rm test.log

