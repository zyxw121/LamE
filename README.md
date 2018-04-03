# LamE
The LAMbda Evaluater (LamE) compiles a small Scheme-like functional langauge to the lambda calculus. 
Development is ongoing, all features are liable to change.

# Usage
Build with [Stack](https://docs.haskellstack.org/en/stable/README/).

```
  $ git clone https://github.com/zyxw121/LamE.git && cd LamE
  $ stack setup && stack build  
```

This builds the executable `LamE` in the `.stack-work` folder. You can move it somewhere else and call it directly, or call it from Stack with `stack exec LamE [args]`.

The executable `LamE` by default takes one LamE source program as an argument and prints the result to standard output.

```
  $ LamE "val y = 4; let val x = 1 in + y x" 
    \z.((z (\f x.(f (f (f (f (f x))))))) (\f x.x))
```

The options `--bnf` and `--hnf` attempt to reduce the result to beta-normal form and head-normal form, respectively. Note that if the result has no normal form, reduction will never terminate.
```
$ LamE "let rec x = 1 in x" --bnf
  (\f.((\x.(f (x x))) (\x.(f (x x))))) \x z.((z (\f x.(f x))) (\f x.x))
  \a.((a (\b c.(b c))) (\b c.c))
```

The options `--int, --bool, --char, --string` attempt to decode the result. If the result is not of the appropriate type termination is not guaranteed.

```
  $ LamE "val y = 4; let val x = 1 in + y x" --int
    \z.((z (\f x.(f (f (f (f (f x))))))) (\f x.x))
    5
```

The options `-i FILE` and `-o FILE` allow reading from and writing to files.

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
