#!/usr/bin/env bash

# Fabric tests
cd resources/tests/FabricTests || exit 1

bash IntContainerTest.sh
bash MultipleConstructorsTest.sh
bash TransactionInConstructorTest.sh

# Yul tests -- note that these only test if the compiler runs without an error
for test in resources/tests/YulTests/*.sh
do
  echo "running Yul Test $test"
  bash "$test"
done
