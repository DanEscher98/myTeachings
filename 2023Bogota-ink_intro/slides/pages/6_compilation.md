# The compilation process

---
hideInToc: true
---

# The Abstract Machine

- is not a runtime, and does not have any runtime overhead, but is a computing
  model abstraction,
- contains concepts such as memory regions (stack, …), execution semantics, …
- knows and sees things your CPU might not care about,
- is de-facto a contract between you and the compiler,
- and exploits all of the above for optimizations.

---
hideInToc: true
---

# The compilation process

```mermaid
stateDiagram-v2
    direction LR
    RS: Source code

    HIR: Rust HIR
    note right of HIR: -Z unpretty=hir-tree

    MIR: Rust MIR
    note left of MIR: -Z unpretty=mir\n--emit=mir 
    
    IR: LLVM IR
    note right of IR: --emit=llvm-ir

    ASM: Machine code

    [*] --> RS
    RS --> HIR: Parsing and Desugaring
    HIR --> MIR: Type checking
    MIR --> IR: Borrow cheking \n Optimization
    IR --> ASM: Optimization
```

