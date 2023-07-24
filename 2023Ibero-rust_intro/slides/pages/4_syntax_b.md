# Basics of syntax (2nd)

---
level: 2
---

# Ownership rules
1. Each value in `Rust` has a variable that's called its owner.
$$\forall x, \exists v : v\:\mathcal{O}\:x$$

2. There can only be one owner at a time.
$$t \rightarrow \nexists v' : v' \neq v$$

3. When the owner goes out of scope, the value will be dropped.
$$\text{End}(v) \rightarrow \text{Drop}(x)$$

```rust{all|1-2|4,6|5|all}
// String is allocated on the heap and owned by `x`
let a = String::from("I ♥️ Linux");
{
    let b = a; // Ownership is transferred from `x` to `v`
} // `b` goes out of scope so String is dropped
println("{}", a); // Compile Error: `a` is invalid
```

---
level: 2
---

# Borrow rules
1. At any given time, you can have either one mutable reference or any
   number of immutable references.
$$t \rightarrow (\exists! r_m : r_m\:\mathcal{B}\:x) \oplus (\exists r_i : r_i\:\mathcal{B}\:x)$$

2. References must always be valid.
$$\forall r, \neg \diamond (r \:\mathcal{R}\: \varnothing)$$

```rust {all|1,3-6|1,7,8,10|2,9|all}
let mut x = String::from("Hello");
let b = &str;
{
    let bm = &mut x;
    bm.push_str(", world!");
} // Mutable borrow ends here
{
    let b1 = &x; let b2 = &x;
    b = b1.to_owned();  // Error: b1 doesn't live enough
} // Immutable borrow ends here
```
