#!/usr/bin/env bash

# Fabric tests
## cd resources/tests/FabricTests || exit 1

## todo/iev: commenting this out to hopefully avoid some redundant work and debug just the ganache part of this
#bash IntContainerTest.sh
#bash MultipleConstructorsTest.sh
#bash TransactionInConstructorTest.sh

echo "--- iev --- current working directory:"
pwd

# Yul tests -- note that these only test if the compiler runs without an error
for test in resources/tests/YulTests/*.sh
do
  echo "running Yul Test $test"
  bash "$test"
done

# todo: rename this file "yul tests" or something to reflect what it actually contains