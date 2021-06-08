#!/bin/bash

# Fabric tests
cd resources/tests/FabricTests || exit 1

bash IntContainerTest.sh
bash MultipleConstructorsTest.sh
bash TransactionInConstructorTest.sh
