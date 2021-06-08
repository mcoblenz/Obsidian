#!/bin/bash

sbt ++$TRAVIS_SCALA_VERSION assembly

# Fabric tests
cd resources/tests/FabricTests || exit 1

bash IntContainerTest.sh
bash MultipleConstructorsTest.sh
bash TransactionInConstructorTest.sh
