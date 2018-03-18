IPLI.hs
===

Iikanji ProLog Interpreter written in Haskell

# Specs

## Tokens

```
token    ::= atom | variable | number | operator | string |
           | '(' | ')' | '[' | ']'
atom     ::= lower (lower | upper | digit | '_')*
           | (one of "~@#$^&*-+=\\/:?<>")+
           | '!' | ',' | '.' | ';'
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

## Predicates

```
true/0
fail/0
call/1
,/2
;/2
=/2
\=/2
var/1
nonvar/1
atom/1
number/1
integer/1
float/1
compound/1
==/2
\==/2
is/2
=</2
</2
>=/2
>/2
=:=/2
=\=/2
asserta/1
assertz/1
op/3
current_op/3
\+/1
once/1
repeat/0
atom_length/2
atom_concat/3
```

## Functions

```
+/1
-/1
\/1
^/2
<</2
>>/2
**/2
div/2
mod/2
rem/2
+/2
-/2
*/2
//2
```

## Future work

- Determinism (I don't understand exactly about prolog's determinism)
- Rational number
- More fluent error messages
- More test

## wontfix

- dynamic/static
