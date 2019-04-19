#!/usr/bin/env bash

cd ../../../
# First generate int container java code
sbt "runMain edu.cmu.cs.obsidian.Main resources/tests/compilerTests/IntContainer.obs"
# Then go to run the tests
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
        echo "The old value in the IntContainer is ${testVals[i]}, but the returned value is $res"
        exit 1
    fi
    echo "      pass"
done

./down.sh
cd ../
rm -rf IntContainer