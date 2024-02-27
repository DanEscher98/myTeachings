# Core features

---
level: 2
---

| Rust Smart Pointer    | C++ Counterpart or Equivalent Concept                     |
| :-------------------- | :-------------------------------------------------------- |
| `option::Option<T>`   | `std::optional<T>`                                        |
| `boxed::Box<T>`       | `std::unique_ptr<T>`                                      |
| `rc::Rc<T>`           | `std::shared_ptr<T>`                                      |
| `sync::Arc<T>`        | `std::shared_ptr<T>` (thread-safe)                        |
| `sync::Weak<T>`       | `std::weak_ptr<T>`                                        |
| `sync::RwLock<T>`     | `std::shared_mutex` $+$ `std::unique_ptr<T>` (write lock) |
| `sync::Mutex<T>`      | `std::mutex<T>` $+$ `std::unique_ptr<T>`                  |
| `cell::RefCell<T>`    | `std::shared_ptr<T>` (with a synchronization mechanism)   |
| `cell::Cell<T>`       | N/A                                                       |

---
level: 2
---

| Rust Smart Pointer    | C++ Counterpart or Equivalent Concept                     |
| :-------------------- | :-------------------------------------------------------- |
| `cell::Ref<T>`        | N/A                                                       |
| `borrow::Cow<T>`      | N/A (Copy-on-write behavior achieved by other means)      |
| `pin::Pin<T>`         | N/A (Rust-specific concept)                               |

---
level: 2
---

# An unsafe case study

```rust{all|5,16,19|7-11|12,13|17,18|all}
/// A function that is opaque to the optimizer, used to prevent the compiler from
/// optimizing away computations in a benchmark.
pub fn black_box<T>(dummy: T) -> T { // criterion::black_box
    unsafe {
        // let ret = std::ptr::read_volatile(&dummy);
        let ret = {
            assert_unsafe_precondition!(
                "ptr::read_volatile requires that the pointer argument is aligned and non-null",
                [T](dummy: *const T) => !dummy.is_null() && // dummy.is_aligned()
                    ((dummy.as_ptr() as usize) % mem::align_of::<T>()) == 0
            );
            // semantics as C11: the compiler is instructed to fetch the value from memory
            intrinsics::volatile_load(dummy)
        };

        // std::mem::forget(dummy);
        // A 0-cost wrapper to inhibit compiler from automatically calling `T`’s destructor.
        let _ = ManuallyDrop::new(dummy); 
        ret
    }
}
```
