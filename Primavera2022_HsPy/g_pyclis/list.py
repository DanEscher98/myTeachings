#!/usr/bin/python3

import argparse
import os
import sys
from argparse import Namespace
from typing import Any

import termcolor


def args_namespace() -> Namespace:
    parser = argparse.ArgumentParser(
        prog="list",
        usage="%(prog)s [options] path",
        description="List contents of a directory",
        epilog="Happy listing!",
    )
    parser.add_argument("Path", metavar="path", type=str, help="the path to list")
    parser.add_argument(
        "-l", "--long", action="store_true", help="enable the long listing mode"
    )
    parser.add_argument(
        "-a", "--all", action="store_true", help="do not ignore entries starting with ."
    )
    return parser.parse_args(sys.argv)


if __name__ == "__main__":
    namespace: argparse.Namespace = args_namespace()
    # print(vars(namespace))
    if not os.path.isdir(path := namespace.Path):
        raise Exception(f"{namespace.Path} is not a dir.")
    for file in os.listdir(namespace.Path):
        line: Any = file
        if file[0] == "." and not namespace.all:
            continue
        if os.path.isdir(file):
            line: Any = termcolor.colored(line, "blue")
        if namespace.long:
            size: int = os.stat(os.path.join(path, file)).st_size
            line: Any = f"{size:10d} {line}"
        sys.stdout.write(f"{line}\n")
