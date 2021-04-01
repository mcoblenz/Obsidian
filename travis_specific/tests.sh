#!/bin/bash

# Fabric tests
cd resources/tests/FabricTests || exit 1

bash IntContainerTest.sh
bash MultipleConstructorsTest.sh
bash TransactionInConstructorTest.sh

cd ../../../

# Yul tests -- note that these only test if the compiler runs without an error
cd resources/tests/YulTests || exit 1

echo "running Yul tests, currently at $( pwd -P)"
for test in *.sh
do
  echo "running Yul Test $test"
  bash "$test"
done
