#!/usr/bin/env bash

source "lib.sh"

init "InterfaceUse"

check "./invoke.sh -q f" "\000"
check "./invoke.sh -q g" "\001"

cleanup "InterfaceUse"

