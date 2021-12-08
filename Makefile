# This Makefile builds the compiler into a jar file, which bin/obsidianc invokes.

all: obsidianc

obsidianc:
	sbt assembly

notest:
	sbt 'set assembly / test := {}' assembly
