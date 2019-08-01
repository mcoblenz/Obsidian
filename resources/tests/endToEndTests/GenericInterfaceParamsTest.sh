#!/usr/bin/env bash

source "lib.sh"

init "GenericInterfaceParams"

check "./invoke.sh -q f" "test"
check "./invoke.sh -q g" "180"

cleanup "GenericInterfaceParams"

