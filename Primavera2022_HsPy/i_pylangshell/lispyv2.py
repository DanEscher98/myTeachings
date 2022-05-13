#!/usr/bin/python3
import os
import re
import sys


def parser(str_program):
    match str_program:
        case ["(", expression, ")"]:
            return eval(expression)
        case ["'", string, "'"]:
            return string
        case _:
            try:
                return int(str_program)
            except ValueError:
                try:
                    return float(str_program)
                except ValueError:
                    raise ValueError(f"{str_program} is not a number")


def eval(expression):
    match expression:
        case ["+", a_exp, b_exp]:
            return parser(a_exp) + parser(b_exp)
        case ["-", a_exp, b_exp]:
            return parser(a_exp) - parser(b_exp)
        case _:
            return None
