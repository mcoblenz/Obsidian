#!/bin/bash

npm install -g npm
npm install
npm install -g ganache-cli
npm audit fix

sudo snap install jq # a commandline JSON tool
sudo apt install rhash
sudo apt install libdigest-sha3-perl
