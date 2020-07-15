#!/usr/bin/env bash

cd ../../../

sbt "runMain edu.cmu.cs.obsidian.Main --yul resources/tests/compilerTests/IntContainer.obs"

cd IntContainer

solc --strict-asembly IntContainer.yul