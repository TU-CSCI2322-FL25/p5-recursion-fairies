# Commands:

.PHONY: build init test clean doc deploy stage

build:
	ghc Main.hs
all: build test

# Cleaning commands:
clean:
	rm -f *.hi
	rm -f *.o

setup:
	cabal install ansi-terminal
