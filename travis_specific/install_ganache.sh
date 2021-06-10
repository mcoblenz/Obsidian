#!/bin/bash

npm install -g npm
npm install -g ganache-cli
npm audit fix

sudo snap install jq # a commandline JSON tool

sudo apt update
sudo apt install libcryptx-perl # perl crypto library that implements Keccak256
