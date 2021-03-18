#!/usr/bin/env bash

cd "$TRAVIS_BUILD_DIR" || exit 1

# Ganache Tests -- these actually build the compiled Yul via Truffle then run it via Ganache

ANY_FAILURES=0

for test in resources/tests/GanacheTests/*.sh
do
  echo "running Ganache Test $test"
  TEST_RESULT=$($test)
  echo "$TEST_RESULT"

  if [ $? -ne 0 ] ; then ANY_FAILURES=1 ; fi
done

exit $ANY_FAILURES

