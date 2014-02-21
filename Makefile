.PHONY: all
all: log

log: test.lisp
	acl2 < test.lisp | tee log

test.lisp: Tests.hs dist/setup-config
	runhaskell -W Tests.hs

Factorial.lisp Factorial.c: Factorial.hs dist/setup-config
	runhaskell -W Factorial.hs

dist/setup-config: src/Ivory/Compile/ACL2.hs src/Ivory/Compile/ACL2/*.hs
	cabal build
	cabal install

.PHONY: clean
clean:
	cabal clean
	-rm *.c *.h
	-rm *.cps1
	-rm *.cps2
	-rm *.rtl
	-rm *.lisp
	-rm log

