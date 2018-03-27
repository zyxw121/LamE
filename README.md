# LamE
The LAMbda Evaluater (LamE) compiles a small Scheme-like functional langauge to the lambda calculus. 


# Usage
Build with [Stack](https://docs.haskellstack.org/en/stable/README/).

```
  $ git pull https://github.com/zyxw121/LamE && cd LamE
  $ stack setup && stack build  
```

This builds the executable `LamE` in the `.stack-work` folder. You can move it somewhere else and call it directly, or call it from Stack.

```
  $ LamE source.lm
```
or
```
  $ stack exec LamE source.lm
```
The executable `LamE` takes one LamE source file as an argument and prints the result to standard output.

```
  $ echo "(let x = 1 in 4 + x)" > source.lm
  $ LamE source.lm
    5
```


