#!/usr/bin/env bash

# Ganache Tests -- these actually build the compiled Yul via Truffle then run it via Ganache
for test in resources/tests/GanacheTests/*.sh
do
  echo "running Ganache Test $test"
  bash "$test"
done