#!/usr/bin/env bash

set -e

source "lib.sh"

init "GenericsStateVariables"

check "./invoke.sh -q g" "14"
check "./invoke.sh -q stateTestTrue" "\001"
check "./invoke.sh -q stateTestFalse" "\000"
check "./invoke.sh -q testStoreOwned" "\001"
check "./invoke.sh -q testStoreOwnedNotOwned" "\001"
check "./invoke.sh -q testStoreShared" "\001"
check "./invoke.sh -q testStoreSharedNotShared" "\001"
check "./invoke.sh -q testStoreUnowned" "\001"
check "./invoke.sh -q testStoreUnownedNotUnowned" "\001"
check "./invoke.sh -q testStoreStateShared" "\001"

cleanup "GenericsStateVariables"

