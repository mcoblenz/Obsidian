#!/bin/bash


# this script is kind of a hack but it works for now and saves a rewrite of
# the run_obs_yul script

for f in $(run_obs_yul.sh | tail -n +2)
do
    echo "---------------------------"
    echo $f
    echo "---------------------------"
    run_obs_yul.sh "$f"
done
