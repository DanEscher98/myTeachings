#!/usr/bin/python3
from __future__ import annotations

from typing import Any, Callable, Protocol, TypeVar

M = TypeVar("M")
A = TypeVar("A")
B = TypeVar("B")
G = TypeVar("G")


class Monad(Protocol):
    def fmap(self, func: Callable[[Any], Any]):
        pass

    def bind(self: M, func: Callable[[Any], M]):
        pass


class Group(Protocol):
    neutral: Any

    def add(self, other: Group) -> Group:
        pass

    def inverse(self) -> Group:
        pass

    def eq(self, other: Group) -> bool:
        pass


class Integer(Group):
    neutral = None

    def __init__(self, value: int):
        self.value = value

    def add(self, other) -> Integer:
        return Integer(self.value + other.value)

    def inverse(self) -> Integer:
        return Integer(self.value * -1)

    def eq(self, other) -> bool:
        return self.value == other.value


def proof_equal_inv(value: Group) -> bool:
    return value.inverse().add(value).eq(Integer(0))
