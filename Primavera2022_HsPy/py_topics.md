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
    - [keywords](https://realpython.com/python-keywords/)
    - [PEP3099:ThingThatWillNotChange](https://peps.python.org/pep-3099/)
    - [PyPi:pytest](https://pypi.org/project/pytype/)
    - [PythonZen](https://www.codementor.io/@abdurrahmaanj/the-zen-of-python-as-related-by-masters-1adi3kuiwy)

### Control Flow and Operator Keywords: 
- words: `if, elif, else, and, or, not, in, is, True, False, None`

### Iteration Keywords:
- words: `for, while, break, continue, else`
- topics: Iterators, Functional
- links:
    - [Functional](https://realpython.com/python-functional-programming/)
    - [Iterators](https://realpython.com/python-for-loop/)
    - [Docs:Functional](https://docs.python.org/3/library/functional.html)
    - [Docs:Datatypes](https://docs.python.org/3/library/datatypes.html)
    - [PickleModule](https://realpython.com/python-pickle-module/)

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
```
- links:
    - [Context Managers](https://realpython.com/python-with-statement/)
    - [Decorators](https://realpython.com/primer-on-python-decorators/)
    - [Descriptors](https://realpython.com/python-descriptors/)
    - [Type Checking](https://realpython.com/python-type-checking/)
    - [Dataclasses](https://realpython.com/python-data-classes/)
    - [OOP Concepts](https://realpython.com/instance-class-and-static-methods-demystified/)
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
    - [Requests](https://realpython.com/python-requests/)
    - [API](https://realpython.com/python-api/)
    - [REST](https://realpython.com/api-integration-in-python/)
    - [Weather CLI](https://realpython.com/build-a-python-weather-app-cli/)

### Returning Keywords:
- words: `return, yield`
- topics: Scope, Iterators

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

### Exception-Handling Keywords:
- words: `try, except, raise, finally, else, assert`
- topics: Exceptions, Testing
- libraries: unittest
- links:
    - [Testing](https://realpython.com/python-testing/)

### Asynchronous Programming Keywords:
- words: `async, await`
- libraries: threading, asyncio, socket
- links:
    - [GIL](https://realpython.com/python-gil/)
    - [Memory managment](https://realpython.com/python-memory-management/)
    - [AsyncIO](https://realpython.com/async-io-python/)
    - [AsyncFeatures](https://realpython.com/python-async-features/)
    - [Docs:Networking](https://docs.python.org/3/library/ipc.html)
    - [Medium:MultiThreading](https://towardsdatascience.com/multithreading-multiprocessing-python-180d0975ab29)
