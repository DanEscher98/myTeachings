# Rust Workshop (June 2023)

This directory contains the material used in a workshop I gave at the
Iberoamericana University, Mexico City. It was part of a Polkadot Hub event for
developers.

## Outline

### Block 1: Introduction to Rust (1.5 hours)

- [ ] Introduction (15 minutes)
    - [ ] Welcome and overview of the workshop
    - [x] Goals of the workshop
        1. Familiarity with `cargo` workflow (TDD, deps)
        2. Understand `mods` & project structure
        3. Get an panoramic view of the Rust ecosystem
        4. Gain intuitions when reading source code
    - [ ] Why Rust?
    - [x] Brief historic context

- [ ] Rust Basic Tooling (15 minutes)
    - [ ] Install [rustup](https://rustup.rs/)
    - [x] Directory layout and explanation
    - [x] Cargo toml file
    - [ ] Rust Documentation and Community Resources
    - [ ] Live coding: hello world

- [ ] Rust Basics 1 (30 minutes)
    - [ ] Variables and Data Types
    - [ ] Control Flow (If, Loop, While, For)
    - [ ] Functions and Modules
    - [x] Live coding: collatz, test, docs, bench, watch

- [ ] Rust Basics 2 (30 minutes)
    - [x] Ownership, Borrowing, and Lifetimes
    - [ ] Structs, Enums, and Pattern Matching (ADTs)
    - [ ] Error Handling (Option & Result)
    - [ ] Template of a `mod`
    - [ ] Live coding: Merkle Tree

### Block 2: Rust in Depth (2 hours)

- [ ] Rust's Killer Features (30 minutes)
    - [ ] Zero-cost abstractions
    - [ ] Memory safety without garbage collection
    - [ ] Smart pointers
    - [ ] Concurrency without data races
    - [ ] Pattern matching
    - [ ] How is Rust secure?
    - [ ] Understanding `unsafe`
    - [ ] Live coding: Demonstrating Rust's killer features

- [ ] Rust compilation process (30 minutes)
    - [ ] Understanding MIR using Compiler Explorer
    - [ ] Procedural Macros in Rust
    - [ ] Traits and Generics
    - [ ] Live coding: Exploring MIR and Macros

- [ ] Exercise (1 hour)
    - [ ] Transition machine
    - [ ] Merkle Tree
    - [x] WASM compile
    - [x] unsafe feature

- [ ] Work ongoing and beyound (15 min)
    - [ ] Ferrosystems specification
    - [ ] MIR formalization

- [ ] Wrap Up and Q&A (15 minutes)
    - [ ] List possible questions

## REFERENCES
- [Cargo Book](https://doc.rust-lang.org/cargo/index.html)
- [The Rust Programming Language](https://doc.rust-lang.org/book/)
- [Rust Design Patterns](https://rust-unofficial.github.io/patterns/)
- [The little book of Rust books](https://lborb.github.io/book/title-page.html)
- [Awesome Rust](https://github.com/rust-unofficial/awesome-rust)
