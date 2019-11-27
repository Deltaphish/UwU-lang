# UwU-lang
UwU-lang is a imperative programming language which aims to compete with brainf*ck
to be the most painfull programming language to read and write in.

UwU-lang's parser and evaluator is written haskell with no external dependencies
except the standard library

Currently the only way to run UwU files is via the interpreter, which
can be built by following the instructions in Build or downloaded from
the releases. A UwU-lang compiler is currently under works.

The language is not stable, both in semantics and operation so it is not
advised to use UwU-lang in production.

## Code example:
finds the first Fibonacci number above 100
~~~~
a iws 1
b iws 1
c iws 0

OwO *notices 100 gweatew twan a *
    c iws b 
    b iws b pwus a
    a iws c
stawp

nuzzels a`
~~~~

Documentation can be found in the wiki

## Build:

1. Make sure you have the haskell compiler [ghc](https://www.haskell.org/downloads) installed
2. pull the repository
3. run ghc main.hs -O2 -o UwU

## Usage:

run a UwU file by using it as an argument with the interpeter:
`UwU test.uwu`
