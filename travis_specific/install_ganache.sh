#!/bin/bash

npm install -g npm
npm install
npm install -g ganache-cli
npm audit fix

apt-get install jq # a commandline JSON tool
