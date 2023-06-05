// CWE-762, or "Mismatched Memory Management Routines," is a common vulnerability in C and C++
// programs where memory is allocated with one function and deallocated with another, leading to
// memory leaks or other memory-related issues.
fn cwe_762() {
    let mut v = Vec::new(); // allocate memory for a new vector
    v.push(1);              // add an element to the vector
    let x = &v[0];          // create a reference to the first element of the vector
    v.push(2);              // add another element to the vector
    println!("{}", x);      // this will not compile
}

fn main() {
    cwe_762();
}
