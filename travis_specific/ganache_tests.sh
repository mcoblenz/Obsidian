#!/bin/bash

# note: this won't be set locally so either set it on your machine to make
# sense or run this only via travis.
cd "$TRAVIS_BUILD_DIR" || exit 1

# Ganache Tests -- these actually build the compiled Yul via Truffle then
# run it via Ganache

ANY_FAILURES=0

for test in resources/tests/GanacheTests/*.sh
do
  echo "---------------------------------------------------------------"
  echo "running Ganache Test $test"
  echo "---------------------------------------------------------------"

  "$test"
  if [ "$?" -ne 0 ]
  then
      ANY_FAILURES=1
  fi
done

exit "$ANY_FAILURES"
