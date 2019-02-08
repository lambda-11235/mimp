
# MImp

MImp is an minimal imperative programming language. Its single data structure is
an infinite array of natural numbers (a.k.a. unsigned integers). The language
includes arithmetic and boolean expressions. Values in these expressions are
either natural number literals, dereferences into the array (written `[<expr>]`
where `<expr>` is itself an arithmetic expression), or the address of an
instruction pointed to by label.

Statements take on several forms. The first form is a label declaration (denoted
`<label>:`), which declares a label that points to the next instruction. The
next form is an assignment of an arithmetic expression into the array. There are
two jump (a.k.a. goto) forms. Each of these forms takes a value and interprets
it as a code location to jump to. Invalid code locations result in undefined
behavior (although labels are guaranteed to always point to valid locations).
The `jif` form also takes a boolean condition, and only jumps if the condition
is true. The `read` stores a user entered value into the array. Finally, the
`print` form prints a value.

It is important to note that the program instructions and array data structure
are stored separately. Thus, MImp does **not** emulate a von Neumann assembly
language.

## Example Execution

```
> stack build                                                                                                                                1 ↵
...
> stack exec mimp examples/fact.mimp
6
720
```

## Syntax

Comments begin with `#` and extend to the end of the line. The EBNF grammar for
MImp is as follows

```
program = { statement } ;

statement = label , ':'
          | ref , '=' , arith
          | "jmp" , value
          | "jif" , cond , value
          | "read" , ref ;
          | "print" , value ;

arith = prod , [ ('+' | '-') , prod ] ;
prod = term , [ ('*' | '/') , term ] ;
term = value
     | '(' , arith , ')' ;

cond = and , { '|' , and } ;
and = compare , { '&' , compare } ;
compare = arith , '<' , arith
        | arith , '=' , arith
        | arith , '>' , arith
        | [ '~' ] , '(' , cond , ')' ;

value = num | ref | label ;
ref = '[' , arith , ']' ;

num = digit , { digit } ;
digit = ? [0-9] ? ;

label = char , { char | digit } ;
char = ? [a-zA-Z_] ? ;
```

## Semantics

MImp's semantics have been formalized in Coq. These formalizations can be found
in the coq/ directory. Note that this formalism doesn't include the operations
`|`, `=`, `>`, `*`, and `/`. These operations expand as such

```
a | b => ~((~a) & (~b))

a = b => ~(a<b) & ~(a>b)

a > b => b < a

x * y =>
res = 0
tmp = y
rec:
    jif (tmp = 0) stop
    res = res + x
    tmp = tmp - 1
    jmp rec
stop:

# This should be floor(x/y), let me know if I mess something up.
x / y =>
res = 0
tmp = x
rec:
    jif (tmp < y) stop
    res = res + 1
    tmp = tmp - y
    jmp rec
stop:
