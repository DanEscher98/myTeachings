# The Workflow

---
level: 2
---

# Directory layout
```txt {all|1,2,5,9,10|2-4|5-10|6-8|11-12|13-14|15-16|all}
📁 .                        // `cargo new --bin my_project`
├──📄Cargo.toml             // project config
├──📄rust_toolchain.toml    // specify Rust release
├──📄.cargo/config.toml     // compiler config
├──📁src
│  ├──📁my_mod
│  │  └── foo.rs
│  ├── utils.rs
│  ├── lib.rs               // expose pub code
│  └── main.rs              // `cargo run`
├──📁tests                  // `cargo test`
│  └── validation.rs
├──📁example                // `cargo run --example usecase`
│  └── usecase.rs
└──📁benches                // `cargo bench`
   └── performance.rs
```

---
level: 2
---

# Common libraries

```ini {all|1-5,7-10,15,16}
[package]
name = "my_package"
version = "0.1.0"
edition = "2018"
authors = ["Danyiel Colin <amaniel2718@protonmail.com>"]

[dependencies]
anyhow              = "1.0.71"  # flexible `Result`
thiserror           = "1.0.40"  # define custom `Error`
clap                = { version="4.2.7", features=["derive"] }
log                 = "0.4.17"
pretty_env_logger   = "0.4.0"   # colored wrapper
colored             = "2.0.0"

[dev-dependencies]
criterion           = { version = "0.5.1", features = ["html_reports"] }
pretty_assertions   = "1.3.0"   # colored wrapper
```

---
level: 2
---

# The Standard Library

```markdown {1-19|21-37|39-51|53-58|60-65|67-86|all} {maxHeight:'400px'}
## Core Primitive types
- `never`           : (exp) The `!` type, also called “never”.
- `fn`              : Function pointers, like `fn(usize) -> bool`.
- `bool`            : The boolean type.
- `pointer`         : Raw, unsafe pointers, *const T, and *mut T.
- `reference`       : References, &T and &mut T.
- `unit`            : The `()` type, also called “unit”.
- `tuple`           : A finite heterogeneous sequence, `(T, U, ..)`.
- `core::array`     : A fixed-size array, denoted `[T; N]`, for the element
  type, `T`, and the non-negative compile-time constant size, N.
- `core::slice`     : A dynamically-sized view into a contiguous sequence,
  `[T]`. Contiguous here means that elements are laid out so that every element
  is the same distance from its neighbors.
- `core::char`      : A character type.
- `core::str`       : String slices.
- `core::f{32, 64}` : A floating point type (specifically IEEE 754-2008).
- `core::i{8, 16, 32, 64, 128, size}`: The signed integer type.
- `core::u{8, 16, 32, 64, 128}`: The unsigned integer type
- `core::usize`     : The pointer-sized unsigned integer type.

## Common types and Collections
- `primitive`       : This module reexports the primitive types to allow usage
  that is not possibly shadowed by other declared types.
- `core::ascii`     : Operations on `ASCII` strings and characters.
- `core::unicode`   : (exp)
- `core::hash`      : Generic hashing support.
- `core::iter`      : Composable external iteration.
- `core::num`       : Additional functionality for numerics.
- `core::cmp`       : Utilities for comparing and ordering values.
- `core::ops`       : Overloadable operators.
- `core::option`    : Optional values.
- `core::result`    : Error handling with the Result type.
- `alloc::string`   : A `UTF-8`–encoded, growable string.
- `alloc::vec`      : A contiguous growable array type with heap-allocated
  contents, written `Vec<T>`.
- `alloc::collections` : Collection types.
- `alloc::fmt`      : Utilities for formatting and printing `Strings`.

## Smart Pointers, Containers and Memory
- `core::alloc`     : Memory allocation APIs.
- `core::any`       : Utilities for dynamic typing or type reflection.
- `core::borrow`    : A module for working with borrowed data.
- `core::clone`     : The `Clone` trait for types that cannot be "implicitly
    copied".
- `core::cell`      : Shareable mutable containers.
- `core::mem`       : Basic functions for dealing with memory.
- `core::pin`       : Types that pin data to its location in memory.
- `core::ptr`       : Manually manage memory through raw pointers.
- `core::sync`      : Useful synchronization primitives.
- `alloc::boxed`    : The `Box<T>` type for heap allocation.
- `alloc::rc`       : Single-threaded reference-counting pointers.

## Asynchronous and Concurrency
- `core::future`    : Asynchronous basic functionality.
- `core:async_iter` : (exp) Composable asynchronous iteration.
- `core::task`      : Types and Traits for working with asynchronous tasks.
- `std::process`    : A module for working with processes.
- `std::thread`     : Native threads.

## Types and Traits
- `core::assert_matches`: (exp) Contains the unstable `assert_matches` macro.
- `core::error`     : (exp) Interfaces for working with Errors.
- `core::convert`   : Traits for conversions between types.
- `core::default`   : The `Default` trait for types with a default value.
- `core::marker`    : Primitive traits and types representing properties of types.

## `IO`, `OS` and Network features
- `core::intrinsics`: (exp) Compiler intrinsics.
- `core::arch`      : `SIMD` and vendor intrinsics module.
- `core::smid`      : Portable `SIMD` module.
- `core::ffi`       : Utilities related to `FFI` bindings.
- `core::hint`      : Hints to compiler that affects how code should be emitted or
    optimized. Hints may be compile time or runtime.
- `core::panic`     : Panic support in the standard library.
- `core::panicking` : (exp) Panic support for core
- `core::time`      : Temporal quantification.
- `core::net`       : Networking primitives for `IP` communication.
- `std::backtrace`: Support for capturing a stack backtrace of an OS thread
- `std::env`        : Inspection and manipulation of the process’s environment.
- `std::error`      : Interfaces for working with `Errors`.
- `std::fs`         : Filesystem manipulation operations.
- `std::io`         : Traits, helpers, and type definitions for core `I/O`
    functionality.
- `std::net`        : Networking primitives for `TCP/UDP` communication.
- `std::os`         : OS-specific functionality.
- `std::path`       : Cross-platform path manipulation.


[Crate std](https://doc.rust-lang.org/std/#)
```
