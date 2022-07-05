#!/usr/bin/python3

import argparse
import sys
from argparse import Namespace


def get_args() -> Namespace:
    parser = argparse.ArgumentParser(
        prog="echo",
        usage="%(prog)s [options] ARGS...",
        description="Echoes its arguments to stdout",
    )
    parser.add_argument("ARGS", help="args to be echoed", nargs="+")
    parser.add_argument("-n", help="no newline at the end", action="store_true")
    parser.add_argument("-s", help="no space between arguments", action="store_true")
    return parser.parse_args()


if __name__ == "__main__":
    args = get_args()
    for arg in args.ARGS[:-1]:
        print(arg, end="" if args.s else " ")
    print(sys.argv[-1], end="" if args.n else "\n")
