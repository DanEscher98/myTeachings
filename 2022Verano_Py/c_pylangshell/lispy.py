#!/usr/bin/python3
"""
program -> [parse] -> AST -> [eval] -> result
LISP:   LISt Processor
        Lots of Irritating Silly Parentheses
        Lisp Is Syntactically Pure

Allowed expressions:
- variable reference:   symbol
- constant literal:     number
- conditional:          (if test consequence alternative)
- definition:           (define symbol expr)
- procedure call:       (proc args...)
"""

from __future__ import annotations

from dataclasses import dataclass
from typing import TypeAlias, Union

Symbol: TypeAlias = str
Number: TypeAlias = Union[int, float]
Atom: TypeAlias = Union[Symbol, Number]
List: TypeAlias = list
Expr: TypeAlias = Union[Atom, List]


class Env(dict):
    """Environment: a dict of {'key': val} pairs with an outer Env"""

    def __init__(self, param=(), args=(), outer=None):
        self.update(zip(param, args))
        self.outer = outer

    def find(self, var):
        return self if (var in self) else self.outer.find(var)


@dataclass
class Procedure(object):
    parms: List
    body: Expr
    env: Env

    def __call__(self, *args):
        return eval(self.body, Env(self.parms, args, self.env))


def read_from_tokens(tokens: list[str]) -> Expr:
    """Read an expression from a sequence of tokens."""
    print(tokens)
    if len(tokens) == 0:
        raise SyntaxError("Unexpected EOF")
    token = tokens.pop(0)
    if token == "(":
        l = []
        while tokens[0] != ")":
            l.append(read_from_tokens(tokens))
        del tokens[0]  # Discard last ')'
        return l
    elif token == ")":
        raise SyntaxError("Unexpected token: )")
    else:
        return atom(token)


def atom(token: str) -> Atom:
    """Tokens become numbers or symbols"""
    try:
        return int(token)
    except ValueError:
        try:
            return float(token)
        except ValueError:
            return Symbol(token)


def tokenize(chars: str) -> list[str]:
    """Convert a string to a list of tokens"""
    return chars.replace("(", "( ").replace(")", " )").split()


def parse(program) -> Expr:
    return read_from_tokens(tokenize(program))


def standard_env() -> Env:
    import math
    import operator as op

    env = Env()
    env.update(vars(math))
    env.update(
        {
            "+": op.add,
            "-": op.sub,
            "*": op.mul,
            "/": op.truediv,
            ">": op.gt,
            "<": op.lt,
            ">=": op.ge,
            "<=": op.le,
            "abs": abs,
            "append": op.add,
            "apply": lambda proc, args: proc(*args),
            "begin": lambda *x: x[-1],
            "car": lambda x: x[0],
            "cdr": lambda x: x[1:],
            "cons": lambda x, y: [x] + y,
            "eq?": op.is_,
            "expt": pow,
            "equal?": op.eq,
            "length": len,
            "list": lambda *x: List(x),
            "list?": lambda x: isinstance(x, List),
            "map": map,
            "max": max,
            "min": min,
            "not": op.not_,
            "null?": lambda x: x == [],
            "number?": lambda x: isinstance(x, Number),
            "procedure?": callable,
            "round": round,
            "symbol?": lambda x: isinstance(x, Symbol),
        }
    )
    return env


global_env = standard_env()


def repl(prompt="lispy> "):
    while True:
        val = eval(parse(input(prompt)))
        if val is not None:
            print(lispstr(val))


def lispstr(exp):
    if isinstance(exp, List):
        return "(" + " ".join(map(lispstr, exp)) + ")"
    else:
        return str(exp)


def eval(x: Expr, env=standard_env()):
    """Eval an expression in a given environment"""
    if isinstance(x, Symbol):  # Variable reference
        return env.find(x)[x]
    if not isinstance(x, List):  # Constant
        return x
    op, *args = x
    match op:
        case "quote":  # Quotation
            return args[0]
        case "if":  # Conditional
            (test, conseq, alter) = args
            exp = conseq if eval(test, env) else alter
            return eval(exp, env)
        case "define":  # Definition
            (symbol, exp) = args
            env[symbol] = eval(exp, env)
        case "set!":  # Assignment
            (symbol, exp) = args
            env.find(symbol)[symbol] = eval(exp, env)
        case "lambda":  # Procedure
            (parms, body) = args
            return Procedure(parms, body, env)
        case "print":
            value = eval(args, env)
            print(value)
            return value
        case _:
            proc = eval(op, env)
            vals = map(lambda arg: eval(arg, env), args)
            return proc(*vals)


if __name__ == "__main__":
    repl()

# References
# - http://norvig.com/lispy.html
