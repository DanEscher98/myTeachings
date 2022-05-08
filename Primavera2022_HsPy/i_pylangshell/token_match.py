#!/usr/bin/python3

tokens1 = ["(", "print", "'Hello World'", ")"]
tokens2 = ["(", "+", "2", "3", ")"]


def eval(tokens):
    match tokens:
        case ["(", "print", msg, ")"]:
            print(msg)
        case ["(", "+", a, b, ")"]:
            print(int(a) + int(b))


for tok in [tokens1, tokens2]:
    eval(tok)
