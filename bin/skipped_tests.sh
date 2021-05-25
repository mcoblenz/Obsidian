#!/bin/bash

comm -13 <(ls resources/tests/GanacheTests/*.json | xargs basename -s '.json') <(ls resources/tests/GanacheTests/*.obs | xargs basename -s '.obs')
