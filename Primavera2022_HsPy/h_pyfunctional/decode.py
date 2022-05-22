#!/usr/bin/python3

import sys


def decode(filename) -> str:
    letters = []
    with open(filename, "r") as file:
        for line in file.readlines():
            chrs_line = map(lambda b: chr(int(b, 2)), line.split())
            letters.extend(list(chrs_line))
    return "".join(letters)


if __name__ == "__main__":
    msg = decode(sys.argv[1])
    print(msg.replace(". ", ".\n").strip())
