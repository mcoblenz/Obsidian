#!/bin/bash


# this is a wrapper on the run_obs_yul script, which produces yul for the ganache tests programs
# without actually running them, that runs it on all of the test programs in a batch mode. it is
# kind of a hack but it works for now and saves a rewrite of the run_obs_yul script.

for f in $(run_obs_yul.sh | tail -n +2)
do
    echo "---------------------------"
    echo $f
    echo "---------------------------"
    run_obs_yul.sh "$f"
done
