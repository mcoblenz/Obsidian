#!/bin/bash

sudo apt update
sudo apt install libcryptx-perl # perl crypto library that implements Keccak256
sudo apt install nodejs

npm install -g npm
npm install -g ganache-cli

sudo snap install jq # a commandline JSON tool
