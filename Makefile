.PHONY: all
all: verify

.PHONY: verify
verify: Factorial.hs ivory-backend-acl2
	runhaskell -W Factorial.hs

.PHONY: loop
loop: Loop.hs ivory-backend-acl2
	runhaskell -W Loop.hs

test.log: test.lisp
	acl2 < test.lisp | tee test.log

factorial.log: factorial.lisp
	acl2 < factorial.lisp | tee factorial.log

test.lisp: Tests.hs ivory-backend-acl2
	runhaskell -W Tests.hs

.PHONY: ivory-backend-acl2
ivory-backend-acl2: ivory-backend-acl2/dist/setup-config
ivory-backend-acl2/dist/setup-config: mira ivory-backend-acl2/src/Ivory/Compile/ACL2.hs ivory-backend-acl2/src/Ivory/Compile/ACL2/*.hs
	cd ivory-backend-acl2 && cabal build
	cd ivory-backend-acl2 && cabal install --force-reinstalls

.PHONY: mira
mira: mira/dist/setup-config
mira/dist/setup-config: mira/src/Mira/*.hs
	cd mira && cabal build
	cd mira && cabal install --force-reinstalls

.PHONY: clean
clean:
	cd mira               && cabal clean
	cd ivory-backend-acl2 && cabal clean
	-rm *.cps1
	-rm *.cps2
	-rm *.rtl
	-rm *.lisp
	-rm *.log

