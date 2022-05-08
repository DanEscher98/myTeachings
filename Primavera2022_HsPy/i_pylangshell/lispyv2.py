#!/usr/bin/python3


def tokenize(str_program):
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
