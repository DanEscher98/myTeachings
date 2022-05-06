#!/usr/bin/python3

from memory_profiler import profile


@profile
def fib(n: int) -> int:
    if n < 2:
        return n
    else:
        return fib(n - 1) + fib(n - 2)

print(fib(8))
