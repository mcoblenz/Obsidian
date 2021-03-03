#!/bin/bash

npm install -g npm@7.6.0 # as of 3 march 2021 this is a recent enough npm build to support ganache and truffle
npm update node # i think node ends up being out of date, too?
npm install
npm install -g ganache-cli truffle