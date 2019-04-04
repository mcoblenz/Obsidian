#!/usr/bin/env bash

cd ../../../
cd network-framework
./down.sh
./up.sh -s IntContainer

testVals=(5 7 8 9 10)
for ((i = 0; i < 4; i++)); do
    echo -n "./invoke.sh -q setX ${testVals[i + 1]}"
    res=`./invoke.sh -q setX ${testVals[i + 1]}`
    sleep 2
    if [[ ${res} -ne testVals[i] ]] ; then
        echo
        echo "The old value of IntContainer is ${testVals[i]}, but it returns $res"
        exit 1
    fi
    echo "      pass"
done

./down.sh