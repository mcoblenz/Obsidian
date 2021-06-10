#!/usr/bin/python

import sys

## takes a two's complement 256 as the only argument and returns it as a
## decimal

def twos_comp(val, bits):
    """compute the 2's complement of int value val"""
    if (val & (1 << (bits - 1))) != 0:
        val = val - (1 << bits)
    return val

print(twos_comp(int(sys.argv[1],16),8*32))
