# LamE
The LAMbda Evaluater (LamE) compiles a small Scheme-like functional langauge to the lambda calculus. 


# Usage
Build with [Stack](https://docs.haskellstack.org/en/stable/README/).

```
  $ git clone https://github.com/zyxw121/LamE.git && cd LamE
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
  $ echo "(val y = 4; let val x = 1 in + y x)" > source.lm
  $ LamE source.lm
   \z.(z(\fx.f(f(f(f(fx))))))(\fx.f)
```


