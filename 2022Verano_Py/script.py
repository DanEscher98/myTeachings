#!/usr/bin/python3


import sys
from typing import Iterable


def myenum(arg_iter: Iterable) -> Iterable:
    index = 0
    for i in arg_iter:
        yield (index, i)
        index += 2


if __name__ == "__main__":
    for i, cmd in myenum(sys.argv):
        print(f"\t{i}: {cmd}")
