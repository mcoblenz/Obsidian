#!/usr/bin/env bash

# Fabric tests
cd resources/tests/FabricTests

bash IntContainerTest.sh
bash MultipleConstructorsTest.sh
bash TransactionInConstructorTest.sh

# Yul tests -- note that these only test if the compiler runs without an error
cd ../YulTests
bash EmptyContract.sh

# Ganache Tests -- these actually build the compiled Yul via Truffle then run it via Ganache
cd ../GanacheTests
bash EmptyContract.sh
