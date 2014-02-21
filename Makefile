.PHONY: all
all: test.log

test.log: test.lisp
	acl2 < test.lisp | tee test.log

factorial.log: factorial.lisp
	acl2 < factorial.lisp | tee factorial.log

test.lisp: Tests.hs dist/setup-config
	runhaskell -W Tests.hs

factorial.lisp: Factorial.hs dist/setup-config
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
	-rm *.log

