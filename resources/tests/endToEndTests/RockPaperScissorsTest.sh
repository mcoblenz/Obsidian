#!/usr/bin/env bash
cd ../../../
# First generate int container java code
# sbt "runMain edu.cmu.cs.obsidian.Main resources/case_studies/RockPaperScissors/RockPaperScissors.obs"
# Then go to run the tests
cd network-framework
./down.sh
./up.sh -s RockPaperScissors

./invoke.sh APlay "rock"
sleep 5
./invoke.sh BPlay "paper"
sleep 30
./invoke.sh AReveal "rock"
sleep 30
res=`./invoke.sh -q getAChoice`
echo "A played $res"
sleep 5
res=`./invoke.sh -q getBChoice`
echo "B played $res"
#./down.sh
