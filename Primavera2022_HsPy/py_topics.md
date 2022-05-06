# Python Topics: Understanding the language by its keywords

## Keywords and the `REPL`

### Navigating IPython and Setup
- Managing versions
- tools: pip, pyenv, pyvenv
- functions:
```python
help("keywords")
import this
```
- links:
    - [pyenv](https://realpython.com/intro-to-pyenv/)
    - [pipenv](https://pipenv-fork.readthedocs.io/en/latest/)
    - [keywords](https://realpython.com/python-keywords/)
    - [PEP3099:ThingThatWillNotChange](https://peps.python.org/pep-3099/)
    - [PyPi:pytest](https://pypi.org/project/pytype/)
    - [PythonZen](https://www.codementor.io/@abdurrahmaanj/the-zen-of-python-as-related-by-masters-1adi3kuiwy)
    - [PEPsMustRead](https://www.diegor.it/2017/06/15/the-must-read-pythons-peps/)
    - [WheelsPkg](https://realpython.com/python-wheels/)
    - [C Bindings](https://realpython.com/python-bindings-overview/)
    - [Cython source](https://realpython.com/cpython-source-code-guide/)
    - [PyPyFaster](https://realpython.com/pypy-faster-python/)
    - [VIM Python](https://realpython.com/vim-and-python-a-match-made-in-heaven/)

### Control Flow and Operator Keywords: 
- words: `if, elif, else, and, or, not, in, is, True, False, None`
- topics: Ternary operator
- links:
    - [Identity vs Equality](https://realpython.com/python-is-identity-vs-equality/)
    - [Operators, Expressions](https://realpython.com/python-operators-expressions/)
    - [Null keyword](https://realpython.com/null-in-python/)

### Iteration and Return Keywords:
- words: `for, while, break, continue, else, yield, return`
- topics: Functional, Scope, List comprehension
- objects: Iterable
- links:
    - [Functional](https://realpython.com/python-functional-programming/)
    - [Iterators](https://realpython.com/python-for-loop/)
    - [Generators and yield](https://realpython.com/introduction-to-python-generators/)
    - [Args and Kwargs](https://realpython.com/python-kwargs-and-args/)
    - [Docs:Functional](https://docs.python.org/3/library/functional.html)
    - [Docs:Datatypes](https://docs.python.org/3/library/datatypes.html)
    - [PickleModule](https://realpython.com/python-pickle-module/)
```python
from typing import Iterable, Iterator, Generator
issubclass(Iterator, Iterable)  # True
issubclass(Generator, Iterator) # True
```

### Exception-Handling Keywords:
- words: `try, except, raise, finally, else, assert`
- topics: Exceptions, Testing
- libraries: unittest, logging, inspect
- links:
    - [Testing](https://realpython.com/python-testing/)

### Structure Keywords: 
- words: `def, class, with, as, pass, lambda, @, _`
- topics: OOP, RAII, Macros, Typing, Decorators
- objects: Context Managers, Protocols, Descriptors, Class
    Constructors
- libraries: dataclasses, functools, typing
- functions:
```python
isinstance()
super()
__call__
```
- links:
    - [Context Managers](https://realpython.com/python-with-statement/)
    - [Decorators](https://realpython.com/primer-on-python-decorators/)
    - [Descriptors](https://realpython.com/python-descriptors/)
    - [Type Checking](https://realpython.com/python-type-checking/)
    - [Dataclasses](https://realpython.com/python-data-classes/)
    - [OOP Concepts](https://realpython.com/instance-class-and-static-methods-demystified/)
    - [OOP in Python3](https://realpython.com/python3-object-oriented-programming/)
    - [Metaclasses](https://realpython.com/python-metaclasses/)
    - [super](https://realpython.com/python-super/)
    - [Class Constructors](https://realpython.com/python-class-constructor/)
    - [Inheritance vs Composition](https://realpython.com/inheritance-composition-python/)
    - [PEP44:Protocols](https://peps.python.org/pep-0544/)
    - [PEP673:SelfType](https://peps.python.org/pep-0673/)

### Structural Pattern Matching
- words: `match, case`
- topics: Type Checking
- libraries: requests
- links:
    - [Regex 1](https://realpython.com/regex-python/)
    - [Regex 2](https://realpython.com/regex-python-part-2/)
    - [Requests](https://realpython.com/python-requests/)
    - [API](https://realpython.com/python-api/)
    - [REST](https://realpython.com/api-integration-in-python/)
    - [Weather CLI](https://realpython.com/build-a-python-weather-app-cli/)
    - [Match examples](https://towardsdatascience.com/the-match-case-in-python-3-10-is-not-that-simple-f65b350bb025)

### Import and Variable Handling Keywords:
- words: `import, from, as, del, global, nonlocal`
- topics: Scope, Namespaces, CLI apps
- libraries: os, sys, argparse
- tools:
- functions:
```python
globals()
```
- links:
    - [Scope](https://realpython.com/python-namespaces-scope/)
    - [CLI](https://realpython.com/command-line-interfaces-python-argparse/)
    - [TreeProject](https://realpython.com/directory-tree-generator-python/)

### Asynchronous Programming Keywords:
- words: `async, await`
- libraries: threading, asyncio, socket, queue, pycurl
- links:
    - [GIL](https://realpython.com/python-gil/)
    - [Memory managment](https://realpython.com/python-memory-management/)
    - [AsyncIO](https://realpython.com/async-io-python/)
    - [AsyncFeatures](https://realpython.com/python-async-features/)
    - [Sockets](https://realpython.com/python-sockets/)
    - [Docs:Networking](https://docs.python.org/3/library/ipc.html)
    - [Medium:MultiThreading](https://towardsdatascience.com/multithreading-multiprocessing-python-180d0975ab29)
