# Exercises

---

# Use an unstable function

<div class="grid grid-cols-2 gap-10 pt-4">

```ini
# rust_toolchain.toml

[toolchain]
channel = "nightly-2023-06-06"
```

```bash
rustup toolchain install nightly
cargo +nightly build # alternative
```

</div>

```rust
#![feature(intrinsics)]
extern "rust-intrinsic" {
    fn volatile_load<T>(src: *const T) -> T;
}

pub fn black_box<T>(dummy: T) -> T{
    unsafe {
        let ret = volatile_load(&dummy);
        std::mem::forget(dummy);
        ret
    }
}
```

---

# Run a little wasm

```bash
# run wasmi
rustup target add wasm32-unknown-unknown
cargo install wasmi_cli
cargo build --target wasm32-unknown-unknown --release
wasmi_cli target/wasm32-unknown-unknown/release/dummy.wasm --invoke func 1

# read wat
wasm2wat target/wasm32-*/debug/dummy.wasm | bat --language rust
```


<div class="grid grid-cols-2 gap-10 pt-4 -mb-6">

```rust
// src/main.rs
#![no_main]
#![no_std]

#[no_mangle]
pub extern "C" fn _start() -> i32 {
    1234
}
```

```ini
# Cargo.toml
cargo-features = ["profile-rustflags"]

[profile.release]
lto = "fat"         # code size < compile time
codegen-units = 1   # single threaded

[profile.dev]
opt-level = 0       # LLVM optimizations
incremental = false # full compilation each time
rustflags = ["-Z", "mir-opt-level=0"]
```

</div>
