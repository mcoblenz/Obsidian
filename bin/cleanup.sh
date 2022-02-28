#!/bin/bash

rm -f benchmarks*.csv

for f in resources/tests/GanacheTests/*.obs
do
    name=$(basename -s '.obs' "$f")
    rm -f "$name/$name.yul"
    rm -f "$name/$name-pretty.yul"
    [ -d "$name" ] && rmdir "$name"
done
