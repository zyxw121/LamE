A valid `.lm` program is a (possible empty) list of semi-colon separated *definitions* followed by an *expression*.

In approximate BNF:
```
  <expr>  ::= <const> 
            | if <expr> then <expr> else <expr
            | match <expr> as (Var <id>) (<expr>) (App <id> <id>) (<expr>) (Abs <id> <id>) (<expr>)
            | func (<args>) (<expr>)
            | <expr> <expr>
            | let <def> in <expr>

  <def>   ::= val <id> = <expr>
            | rec <id> = <expr>

  <args>  ::= <id> | <id> <args>
```
An identifier `<id>` is a string of letters, numbers, and the symbols \_\-\+\*/\<\>=% with the conditions that:  
* it starts with a letter unless it's one of the single character primitives \+, \-, \*, /, \<, \>, % or the two character primitives ==, =s,=c, \<=, \>=
* it's not one of the keywords: true, false, if, then, else, let, val, rec, func, in, match, as

A const `<const>` can be:  
* an identifier
* a boolean constant: true, false
* a char literal, string literal, or list literal. These are all like in haskell.
