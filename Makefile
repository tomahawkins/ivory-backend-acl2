.PHONY: all
all: test

.PHONY: test
test: Testcases.hs install
	runhaskell -W Testcases.hs

.PHONY: install
install: ivory-backend-acl2

.PHONY: ivory-backend-acl2
ivory-backend-acl2: dist/setup-config
dist/setup-config: src/Ivory/Compile/*.hs src/Ivory/Opts/*.hs src/Mira/*.hs
	cabal clean
	cabal build
	cabal install

.PHONY: clean
clean:
	cabal clean

