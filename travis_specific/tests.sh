#!/usr/bin/env bash

# Fabric tests
cd resources/tests/FabricTests

bash IntContainerTest.sh
bash MultipleConstructorsTest.sh
bash TransactionInConstructorTest.sh

# Yul tests
cd ../YulTests

bash EmptyContract.sh
