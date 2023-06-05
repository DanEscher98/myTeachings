# CWEs addressed by Rust


## CWEs at the 2022 Top 25
- R7  CWE-416: Use After Free - Rust solves this through its ownership model,
  which ensures that memory is not used after it has been freed.
- R19 CWE-119: Buffer Errors

## Other CWEs
- CWE-120: Buffer Copy without Checking Size of Input ('Classic Buffer
  Overflow') - Rust solves this through its use of a borrow checker, which
  ensures that a buffer does not overflow.
- CWE-121: Stack-based Buffer Overflow - Rust solves this through its use of a
  borrow checker, which ensures that a stack buffer does not overflow.
- CWE-122: Heap-based Buffer Overflow - Rust solves this through its use of a
  borrow checker, which ensures that a heap buffer does not overflow.
- CWE-126: Buffer Over-read - Rust solves this through its use of a borrow
  checker, which ensures that a buffer is not read past its end.
- CWE-127: Buffer Under-read
- CWE-131: Incorrect Calculation of Buffer Size
- CWE-170: Improper Null Termination
- CWE-401: Missing Release of Memory after Effective Lifetime - Rust solves
  this through its ownership model, which ensures that memory is automatically
  freed when it is no longer needed.
- CWE-415: Double Free - Rust solves this through its ownership model, which
  ensures that memory is freed only once.


| CWE ID  | Rank | Description                                                              | Status                            |
| :-----: | ---: | :----------------------------------------------------------------------- | :-------------------------------- |
| CWE-119 |      | Improper Restriction of Operations within the Bounds of a Memory Buffer	| Mitigated by borrow checker       |
| CWE-120 |      | Buffer Copy without Checking Size of Input ('Classic Buffer Overflow')	| Eradicated by **borrow** checker  |
| CWE-121 |      | Stack-based Buffer Overflow	                                            | Eradicated by **borrow** checker  |
| CWE-122 |      | Heap-based Buffer Overflow	                                            | Eradicated by **borrow** checker  |
| CWE-125 |      | Out-of-bounds Read	                                                    | Mitigated by borrow checker       |
| CWE-126 |      | Buffer Over-read	                                                        | Eradicated by **borrow** checker  |
| CWE-190 |      | Integer Overflow or Wraparound	                                        | Mitigated by panic! macro         |
| CWE-191 |      | Integer Underflow (Wrap or Wraparound)	                                | Mitigated by panic! macro         |
| CWE-401 |      | Missing Release of Memory after Effective Lifetime	                    | Eradicated by **ownership** model |
| CWE-415 |      | Double Free	                                                            | Eradicated by **ownership** model |
| CWE-416 | 7    | Use After Free	                                                        | Eradicated by **ownership** model |
| CWE-476 |      | NULL Pointer Dereference	                                                | Mitigated by Option enum          |
| CWE-502 |      | Deserialization of Untrusted Data	                                    | Mitigated by Rust's Serde library |
| CWE-676 |      | Use of Potentially Dangerous Function	                                | Mitigated by standard library     |
| CWE-762 |      | Mismatched Memory Management Routines	                                | Eradicated by **ownership** model |
| CWE-943 |      | Improper Neutralization of Special Elements used in an SQL Command       | Mitigated by Rust's Diesel ORM    |


