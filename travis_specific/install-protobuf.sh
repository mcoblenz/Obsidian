#!/bin/sh

set -e
# check to see if protobuf folder is empty
if [ ! -d "${HOME}/protobuf/" ]; then
    mkdir -p "${HOME}/protobuf/"
    wget https://github.com/google/protobuf/releases/download/v3.7.0/protoc-3.7.0-linux-x86_64.zip
    unzip protoc-3.7.0-linux-x86_64.zip
    mv bin/protoc "${HOME}/protobuf/"
else
    echo "Using cached directory."
    fi
