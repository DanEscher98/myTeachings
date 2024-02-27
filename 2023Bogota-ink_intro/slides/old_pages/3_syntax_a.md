# Basics of syntax (1st)

- Control Flow (If, Loop, While, For)
- Functions and Modules
- Structs, Enums, and Pattern Matching (ADTs)
- Error Handling (Option & Result)

---
hideInToc: true
---

# Template of a `mod`


```rust
use utils::Concept;

enum Variant {
    A(f64),
    B(u8, u8)
}

pub struct Data {
    // fields
}

impl Data {
    fn new() -> Self {
        // ...
    }
}

impl<T> Concept<T> for Data {
    // ..
}
```

---
hideInToc: true
---

# `anyhow` and `thiserror`

```rust
use thiserror::Error;

pub type Result<T> = anyhow::Result<T, anyhow::Error>;

#[derive(Error, Debug)]
pub enum RsLispError {
    #[error("Divide by Zero")]
    DivideByZero,

    #[error("NaN: {0}")]
    NotANumber(#[from] std::num::ParseIntError),

    #[error("Parse error: {0}")]
    ParseError(String),
}
```

