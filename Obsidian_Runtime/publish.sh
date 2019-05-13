#!/bin/bash

PUBLISH="$(gradle publish)"
echo "publish result: $PUBLISH"

if [[ $PUBLISH =~ .*SUCCESS.* ]]
   then
       scp -r ../docs/repository gauria@unix.andrew.cmu.edu:~/www/
       rm -rf ~/.gradle/caches/
       exit 0
    else
    exit 1
fi
