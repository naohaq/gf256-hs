# gf256-hs
A library providing arithmetic operations over GF(2^8)

Toy example:
```haskell
$ ghci Data.GF256
> let x = 1111 :: GF256 PP301
> let y = 1112 :: GF256 PP301
> x
227
> y
236
> let z = 1111 :: GF256 PP285
> z
35
> x * y
97
> x + y
15
> x / y
29
> 1 / x
248
> x ^ 254
248
```
