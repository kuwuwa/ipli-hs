IPLI.hs
===

Iikanji ProLog Interpreter written in Haskell

# Specs

## Tokens

```
token    ::= atom | variable | number | operator | string |
           | '(' | ')' | '[' | ']'
atom     ::= lower (lower | upper | digit | '_')*
           | (1 of "
           | '\'' (!'\'' char)* '\''
variable ::= (upper | '_') (lower | upper | digit | '_')*
number   ::= '-'? digit+ ('.' digit+)? (('e' | 'E') ('-')? digit+)?
operator ::= as follows
```

## Operators

```
1200 | xfx | -->, :-
1200 | fx  | -, ?-
1100 | xfy | ;, |
1050 | xfy | ->, *->
1000 | xfy | ,
 990 | xfx | :=
 900 | fy  | \+
 700 | xfx | <, =, =.., =@=, \=@=, =:=, =<, ==, =\=, >, >=, @<, @=<, @>, @>=,
     |     | \=, \==, as, is, >:<, :<
 600 | xfy | :
 500 | yfx | +, -, /\, \/, xor
 500 | fx  | ?
 400 | yfx | *, /, //, div, rdiv, <<, >>, mod, rem
 200 | xfx | **
 200 | xfy | ^
 200 | fy  | +, -, \
 100 | yfx | .
   1 | fx  | $
 ```
