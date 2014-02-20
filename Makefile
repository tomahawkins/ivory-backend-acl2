.PHONY: all
all: Factorial.c

Factorial.c: Factorial.hs dist/setup-config
	runhaskell -W Factorial.hs

PPMDecode.c: PPMDecode.hs dist/setup-config
	runhaskell -W PPMDecode.hs

dist/setup-config: src/Ivory/Compile/ACL2.hs src/Ivory/Compile/ACL2/*.hs
	cabal build
	cabal install

.PHONY: clean
clean:
	cabal clean
	-rm *.c *.h

