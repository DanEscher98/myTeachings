#!/usr/bin/python3
from inspect import currentframe, getframeinfo
from typing import Generator, Iterable, Iterator


class MyHandler:
    def __init__(self, msg):
        self.msg = msg
        if not ((frame := currentframe()) is None):
            print(f"Calling {getframeinfo(frame).function} method")

    def __enter__(self):
        if not ((frame := currentframe()) is None):
            print(f"Calling {getframeinfo(frame).function} method")
        return self.msg

    def __exit__(self, exc_type, exc_val, traceback):
        if not ((frame := currentframe()) is None):
            print(f"Calling {getframeinfo(frame).function} method")
        return True


if __name__ == "__main__":
    with MyHandler("I'm Happy") as hnd:
        print(hnd)
