---
theme: ./theme
class: text-center
highlighter: prism
lineNumbers: false
info: |
  ## Polkadot-Rust template
  Presentation slides for Polkadot events
drawings:
  persist: false
transition: slide-left
title: Rust Workshop
---

# Rust Workshop

Danyiel Colin

<div class="pt-12">
  <span @click="$slidev.nav.next" class="px-2 py-1 rounded cursor-pointer" hover="bg-white bg-opacity-10">
    Let's start! 🦀 <carbon:arrow-right class="inline"/>
  </span>
</div>

<div class="abs-br m-6 flex gap-2">
  <button @click="$slidev.nav.openInEditor()" title="Open in Editor" class="text-xl slidev-icon-btn opacity-50 !border-none !hover:text-white">
    <carbon:edit />
  </button>
  <a href="https://github.com/slidevjs/slidev" target="_blank" alt="GitHub"
    class="text-xl slidev-icon-btn opacity-50 !border-none !hover:text-white">
    <carbon-logo-github />
  </a>
</div>

---
layout: default
---

# Table of contents

<Toc columns=2 mode='all'/>


---
transition: fade-out
---

# ¿Por qué Rust?

- 2003: **LLVM** release
- 2011: **Rust** bootstrap compiler, **LLVM** based
- 2011: **C++** smart pointers (`unique_ptr`, `shared_ptr`, `weak_ptr`)
- Oct-2012 **TypeScript** first appeared
- Sep-2014: **Python** Type Hints (PEP 484)
```python
def greet(name: str) -> str:
    return 'Hello ' + name
```

- May-2015: **Rust** first stable release
- Mar-2022: **Golang** added generics
- Jul-2022: **Carbon** first time presented at a **C++** conference


---

# Directory layout
```
📁 .                        // `cargo new --bin my_project`
├──📄README.md
├──📄Cargo.toml             // project config
├──📄.cargo/config.toml     // compiler config
├──📁src
│  ├──📁my_mod
│  │  ├── func_1.rs
│  │  └── func_2.rs
│  ├── lib.rs               // expose pub code
│  ├── main.rs              // `cargo run`
│  └── utils.rs
├──📁tests                  // `cargo test`
│  └── validation.rs
└──📁benches                // `cargo bench`
   └── performance.rs
```

---

# Common libraries

```toml
[package]
name = "my_package"
version = "0.1.0"
edition = "2018"
authors = ["Daniel Sanchez <amaniel2718@protonmail.com>"]

[dependencies]
anyhow              = "1.0.71"  # flexible `Result`
thiserror           = "1.0.40"  # define custom `Error`
log                 = "0.4.17"
pretty_env_logger   = "0.4.0"   # colored wrapper
colored             = "2.0.0"
clap                = { version="4.2.7", features=["derive"] }

[dev-dependencies]
pretty_assertions   = "1.3.0"   # colored wrapper
criterion           = { version = "0.4", features = ["html_reports"] }
```

---

# benches

```rust
pub fn black_box<T>(dummy: T) -> T {
    unsafe {
        let ret = std::ptr::read_volatile(&dummy);
        // let ret = {
        //     assert_unsafe_precondition!(
        //         "ptr::read_volatile requires that the pointer argument is aligned and non-null",
        //         [T](dummy: *const T) => !dummy.is_null() &&
        //             ((dummy.as_ptr() as usize) % mem::align_of::<T>()) == 0
        //     );
        //     intrinsics::volatile_load(dummy)
        // };

        std::mem::forget(dummy);
        //let _ = ManuallyDrop::new(dummy);
        ret
    }
}
```

---
layout: center
class: text-center
---

# Learn More

[Documentations](https://sli.dev) · [GitHub](https://github.com/slidevjs/slidev) · [Showcases](https://sli.dev/showcases.html)
