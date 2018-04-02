# Syntax

A valid `.lm` program is a (possible empty) list of semi-colon separated *definitions* followed by an *expression*.

## Definitions
A definition assigns a name to an expression, so that it may be used in any following expressions.

There are two types of definitions, value and recursive definitions.

### Value
Value definitions look like:
```
  val x = 4;
  val y = "four";
  val z = ['x',x];
  val w = + x 5;
  val equals4 = func (n) (== n x);
  val a = equals4 w;
```

The right side of a value definition should not contain the identifier on the left side. 
Value definitions correspond to pure substitution. The expression `let x = e1 in e2` is identical to `e2` with all free occurences of `x` replaced with `e1`. 

### Recursive
On the other hand, recursive definitions may reference the identifier they bind.

So the definition `rec x = + 1 x;` is valid. 

We use Church's Y combinator to handle recursive definitions. So the expression `let rec x = e1 in e2` is identical to `let val x = Y(\x.e1) in e2`.

## Expressions
### Identifiers
With a few exceptions, an identifier must start with a letter. The following characters may be letters, digits, or the allowed symbols: "\+\-\*/\<\>=%"

The exceptions are:
* The single character primitives: =, \-, \*, /, %, \<, \>
* The multiple character primitives: ==, =s, =c

### Constants
The Boolean literals are `true` and `false`. The boolean operators are `and`, `or`, and `neg` (negation).

Integer, character and string literals are like Haskell

Use the operators `==`, `=c`, and `=s` for integer, character, and string equality, respectively. Furthermore, there are integer comparison operators: `<`, `<=`, `>=` and `>`, but not for characters or strings (yet!).

Lists can be expressed with square brackets, like Haskell. 

The list operations are `head`, `tail`, `cons`, which are all like in Haskell, and `nil` and `empty`. `nil` is the empty list, while `empty xs` tests whether `xs` is equal to `nil` or not. 

Note that there is no pattern matching on lists, but a similar effect can be achieved with `if` and `empty`.

Given the Haskell expression
```
case l of
  [] -> e1
  (x:xs) -> e2 x xs
```

you can write
```
if empty l then e1 else e2 (head l) (tail l)
```
to get the same effect.


### If statements
Write an if statement like 
```
if cond then e1 else e2
```
It does exactly what you'd expect.

### Lambda matching
In Haskell we could define lambda terms as:

```
data Term = Var String | App Term Term | Abs String Term
```

This defines the 3 `Term` constructions as functions:

```
Var :: String -> Term
App :: Term -> Term -> Term
Abs :: String -> Term -> Term
```

Haskell lets us pattern match on the structure of a `Term`. Consider the expression:

```
case x of
  (Var n) -> e1
  (App s t) -> e2
  (Abs n s) -> e3
```

If `x` evaluates to `Var m` then the value of the above expression is the value of `e1` with `m` substituted for `n`. And so on for the other cases.

LamE has something similar.

We have the primitive functions `Var`, `App`, and `Abs` that construct lambda terms.

Also, we can match on the structure of a lambda term with the syntax:

```
match x as
  (Var n) (e1)
  (App s t) (e2)
  (Abs n s) (e3)
```

Where `n`, `s`, and `t` are identifiers, and the `e`s are expressions.

Note that:
* The linebreaks/indentation are not necessary
* The parentheses are


### Functions
A function definition consists of the word `func` followed by the parameters and body.

The parameters are a sequence of identifiers seperated by spaces. The body is any expression.
The parameters and body must be enclosed in parentheses.

```
func (x) (x)
func (x y z) (if x then + y z else - y z)
```


### Function application
Function application is Haskell style, however partial application is not supported.
Note that infix expressions aren't supported yet. So write `+ 1 2` rather than `1 + 2`.

### Let statements
Let statements allow local definitions.

```
val x = 1;
let val x = 2 in x
```

The above would evaluate to 2.

## Primitive functions
The builtin primitives are:

* Integer operations
  - Addition: \+ 
  - Subtraction: \-
  - Multiplication: \*
  - Integer division: /
  - Modulo: %
* Integer comparisons
  - LT: \<
  - Leq: \<=
  - Equality: ==
  - Geq: \>=
  - GT: \>  
* Boolean operations
  - Negation: `neg`
  - Conjunction: `and`
  - Disjunction: `or`
* Character comparisons
  - Equality: `=c`
* List operations
  - Head: `head`
  - Tail: `tail`
  - Cons: `cons`
  - Test if empty: `empty`
  - Empty list: `nil`
* String comparions
  - Equality: `=s`
* Lambda constructors
  - `Var`
  - `App`
  - `Abs`

## Other things
* The language is case sensitive
* There is no support for comments yet
* Whitespace is _usually_ ignored
