#!/usr/bin/env bash

cd resources/tests/endToEndTests

bash IntContainerTest.sh
bash MultipleConstructorsTest.sh
bash TransactionInConstructorTest.sh
