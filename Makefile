.PHONY: all
all: Factorial.hs dist/setup-config
	runhaskell -W Factorial.hs

dist/setup-config: src/Ivory/Compile/ACL2.hs
	cabal build
	cabal install

.PHONY: clean
clean:
	cabal clean
	-rm Factorial.c
	-rm Factorial.h
	-rm ivory.h
	-rm ivory_asserts.h

