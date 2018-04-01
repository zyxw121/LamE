# LamE
The LAMbda Evaluater (LamE) compiles a small Scheme-like functional langauge to the lambda calculus. 
Development is ongoing, all features are liable to change.

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

# Why?
Curiosity, mainly. We know the untyped lambda calculus is Turing complete, so (loosely speaking), any program and datatype can be encoded as a lambda term. 

In particular, it is possible to encode lambda terms themselves, and algorithms about lambda terms (substitution, reduction, etc). 

That is the purpose of LamE - to convert programs _about_ the lambda calculus to terms _in_ the lambda calculus.

# Features

* First class functions
* Call by value semantics
* Immutable variables
* Lambda term data-type with pattern matching
* No type system

# Examples
Can be found in the `examples` directory.

# Syntax
Mostly Lisp-ish with a bit of Haskell

[See here](SYNTAX.md) for a detailed explanation of the syntax.

# Coming Soon
* Better test coverage
* Better documentation
* Better error messages
* Infix arithmetic
* A REPL
* Some kind of type system?
