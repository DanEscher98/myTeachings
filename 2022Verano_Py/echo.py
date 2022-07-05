#!/usr/bin/python3

import sys

if __name__ == "__main__":
    if sys.argv[1] == "-n":
        print(sys.argv[2], end="")
    else:
        print(sys.argv[1])
