# Commands:

.PHONY: build init test clean doc deploy stage

build:
	ghc --make -O -o cluster Main.hs

prof:
	ghc --make -prof -o cluster Main.hs

all: build test

# Cleaning commands:
clean:
	rm -f cluster
	rm -f *.hi
	rm -f *.o

test: build
	./cluster --test

testq: build
	./cluster --test -q

setup:
	cabal install ansi-terminal
	cabal install drawille
