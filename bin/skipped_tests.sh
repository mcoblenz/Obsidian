#!/bin/bash

# when run from the top level of the repo, this prints out the names of the obsidian files
# in the ganache tests that do not also have a json file paired with them -- that means that
# they won't be run by travis. this also appears in the output of the travis script at the very top.
comm -13 <(ls resources/tests/GanacheTests/*.json | xargs basename -s '.json') <(ls resources/tests/GanacheTests/*.obs | xargs basename -s '.obs')
