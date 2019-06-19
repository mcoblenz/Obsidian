#!/usr/bin/env bash

# Print the usage message
function printHelp() {
  echo "Usage: "
  echo "  upgrade.sh [-v <version>] [-n <init-args>]"
  echo "    -n <arg> - argument to pass when instantiating the chaincode. Pass multiple -n arguments for multiple arguments."
  echo "    -v <arg> - the new version of the chaincode after the upgrade"
  echo "  upgrade.sh -h (print this message)"
}

INIT=""

while getopts "h?c:t:d:f:s:l:i:v:n:" opt; do
  case "$opt" in
  h | \?)
    printHelp
    exit 0
    ;;
  v)
    VERSION=$OPTARG
    ;;
  n)
    if [ -n "$INIT" ]; then
        INIT="${INIT},\"${OPTARG}\""
    else
        INIT="\"${OPTARG}\""
    fi
    ;;
  esac
done

# Looks at all the versions installed on the peer, and picks a higher one
# For example, if there are versions 1.0, 1.1, and 1.2, this function will pick 1.3
get_version() {
    docker exec cli peer chaincode list --installed | tail -1 | while read entry; do
        version_num="$(echo "$entry" | cut -f2 -d"," | cut -f2 -d":" | xargs)"
        new_version="$(echo "$version_num + 0.1" | bc -l)"
        if [[ -z "$VERSION" ]]; then
            VERSION="$new_version"
        else
            if [[ "$(echo "$new_version > $VERSION" | bc -l)" == "1" ]]; then
                VERSION="$new_version"
            fi
        fi

        echo "$VERSION"
    done
}

# Choose a version if one wasn't passed in
if [[ -z "$VERSION" ]]; then
    VERSION="$(get_version)"
fi

docker exec cli scripts/upgrade.sh 0 1 "$INIT" "$VERSION"

