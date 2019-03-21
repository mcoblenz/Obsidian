#!/usr/bin/env bash

if [ ! -f log.txt  -o ! -s log.txt ]; then
    echo "=============================================="
    echo "       log file is not found or empty!"
    echo "       Have you invoked any function?"
    echo "=============================================="
    exit 1
fi

status=`cat log.txt | sed -nr 's/^.*?status:([0-9]+).*$/\1/p'`

guid=`cat log.txt | sed -nr 's/^.*?payload:\"(.*)\".*$/\1/p'`

if [ $status -ne 200 ] ; then
    echo 'Cannot get guid'
    cat log.txt
else
    echo $guid
fi
