#!/bin/bash

./travis_specific/install-protobuf.sh
curl -sSL http://bit.ly/2ysbOFE | bash -s 1.4.1 -s
mv bin/* $HOME/bin/
export PATH=${PATH}:${HOME}/protobuf/
export PATH=${PATH}:${HOME}/bin/

sbt ++$TRAVIS_SCALA_VERSION assembly

# Fabric tests
cd resources/tests/FabricTests || exit 1

bash IntContainerTest.sh
bash MultipleConstructorsTest.sh
bash TransactionInConstructorTest.sh
