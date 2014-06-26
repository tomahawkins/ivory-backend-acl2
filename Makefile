.PHONY: all
all: test

.PHONY: test
test: Testcases.hs install
	runhaskell -W Testcases.hs

.PHONY: install
install: ivory-backend-acl2

.PHONY: ivory-backend-acl2
ivory-backend-acl2: ivory-backend-acl2/dist/setup-config
ivory-backend-acl2/dist/setup-config: mira/dist/setup-config ivory-backend-acl2/src/Ivory/Compile/ACL2.hs
	cd ivory-backend-acl2 && cabal clean
	cd ivory-backend-acl2 && cabal build
	cd ivory-backend-acl2 && cabal install

.PHONY: mira
mira: mira/dist/setup-config
mira/dist/setup-config: mira/src/Mira.hs mira/src/Mira/*.hs
	cd mira && cabal build
	cd mira && cabal install --force-reinstalls

.PHONY: clean
clean:
	cd mira               && cabal clean
	cd ivory-backend-acl2 && cabal clean

