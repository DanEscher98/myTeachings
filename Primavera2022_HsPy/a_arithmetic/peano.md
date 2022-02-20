# Recursion with Peano

Let $P$ be the predecessor function and $S$ the successor. Given the
following implementation in Haskell of the natural numbers:
```haskell
data Natural = Zero | S Natural
```
Each of the following examples are implemented in math and Haskell
syntax, using a **recursive** approach.

## Addition
```haskell
add :: Natural -> Natural -> Natural
add Zero n = n
add (S m) n = add m (S n)
```
$$\text{add}(a, b) = \begin{cases}
    a \quad &, b = 0 \\
    \text{add}(a+1, b-1) \\
\end{cases}$$

## Multiplication
```haskell
mul :: Natural -> Natural -> Natural
mul Zero n = Zero
mul (S m) n = add n (mul m n)
```
$$\text{mul}(m, n) = \begin{cases}
    0 \quad &, m = 0 \\
    \text{sum}(n, \text{mul}(m-1, n)) \\
\end{cases}$$

## Subtraction
```haskell
sub :: Natural -> Natural -> Natural
sub Zero n = n
sub (S m) (S n) = add m (S n)
```
$$\text{sub}(m, n) = \begin{cases}
    m \quad &, n = 0 \\
    ! \quad &, m = 0 \\ 
    \text{sub}(m-1, n-1) \\
\end{cases}$$

## Rest
```haskell
sub :: Natural -> Natural -> Natural
sub Zero n = n
sub (S m) (S n) = add m (S n)
```
$$\text{res}(m, n) = \begin{cases}
    m \quad &, n = 0 \\
    \text{sub}(m-1, n-1) \\
\end{cases}$$




