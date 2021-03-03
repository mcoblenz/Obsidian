#!/usr/bin/env bash

# Fabric tests
## cd resources/tests/FabricTests || exit

## todo/iev: commenting this out to hopefully avoid some redundant work and debug just the ganache part of this
#bash IntContainerTest.sh
#bash MultipleConstructorsTest.sh
#bash TransactionInConstructorTest.sh

# Yul tests -- note that these only test if the compiler runs without an error
cd ../YulTests || exit 1
bash EmptyContract.sh

# Ganache Tests -- these actually build the compiled Yul via Truffle then run it via Ganache
cd ../GanacheTests || exit 1
bash EmptyContract.sh
