# A Simple Compiler
## Mujtaba Aslam
A simple compiler which a software system that translates programs from one form to another.

# Compiler
A compiler for a small arithmetic language built on infix style. It handles numbers, arithmetic operations, boolean values, boolean operations, let-binds, functions, recursion, variables, reference cells, while loops and arrays. The syntax for the compiler is as follows:

```
e ::= (e) | n | b | e1 (+) e2 | if e1 then e2 else e3
    | x | let x : t = e1 in e2
    | e1 (e2) | fun (x:t1) : t2 -> e | fix f (x:t1) : t2 -> e
    | ()
    | (e1, e2) | fst e | snd e
    | [] : t | e1 :: e2 | hd e | tl e | empty e
    | ref e | e1 := e2 | !e | (e1) ; e2
    | while e1 do e2 end
    | new t[n] | e1[e2] | e1 := e2

(+) ::= + | - | * | /
       | == | <= | >= | < | >

t ::= int | bool | t1 -> t2
     | unit | t1 * t2 | [t] | <t> | array<t>

```

It also supports five command flags:

`-lex` -- processes the input source file through the lexing phase and prints the resulting stream of tokens to the console  
`-parse` -- processes the input source file through the parsing phase and prints the resulting abstract syntax tree  
`-step` -- processes the input and prints out every step of evaluation  
`-repl` -- enters repl mode which allows the user to interactively enter expressions and see their final evaluated values
`-help` --  Display this list of options  
`--help` -- Display this list of options  


## Setup Instructions
The compiler uses the core library for OCaml. The ocamlbuild compiler is used to build this program. Make sure the Menhir library for parsing is installed.

## Build Instructions
To build the program simply run `make` in the root directory. To remove the compiled files simply run `make clean`.

## Execution Instructions
Run the compiler program using the command `./compiler.native [flag] [file]` where the file argument are paths to a file that contain an infix style expression. Run the test suite with the command `make test`.

# CLI  
cli takes an amount of arguments and prints them out in order, one in each line. It also supports three command flags:  
`-length` -- prints out the lengths of the arguments instead of the arguments themselves.  
`-help` -- prints out a usage message for cli.  
`--help` -- prints out a usage message for cli.  

## Setup Instructions
CLI uses the core library for OCaml. The ocamlc compiler is used to build this program.

## Build Instructions
To build the program simply run `make` in the root directory. To remove the compiled files simply run `make clean`.

## Execution Instructions
Run the cli program using the command `./cli [flag] [args]`. Run the test suite with the command `make test`

## Hooks
Hooks can be installed to keep commits clean and correct. To install hooks run the `./makehook.sh` command in the githooks directory.

## Change Log  
*Assignment 1*  
**Added**  
* Cli source code  
* Tests for cli   
* Makefile  

**Changed**  
Nothing  

**Known Bugs**  
None  

*Assignment 2*  
**Added**  
* Source code for simple compiler supporting arithmetic operations and booleans

**Changed**  
* Tests for compiler

**Known Bugs**  
None  

*Assignment 3*  
**Added**  
* Source code for lexer and parser to use ocaml libraries to lex and parse
* Source code of compiler to use ocaml lexing and parsing libraries

**Changed**  
* Tests for compiler
* parsel.ml to parser-old.ml
* lexer.ml to lexer-old.ml
* compiler.ml to compiler-old.ml
* The syntax of the language to support infix style rather than S-expressions.

**Known Bugs**  
None  

*Assignment 4*  
**Added**  
* Source code to support let-binds  
* Source code to support functions  
* Source code to support variables
* Source code to support recursion  
* Source code for small step semantics  

**Changed**  
* Tests for compiler  

**Known Bugs**  
None  

*Assignment 5*  
**Added**  
* Source code for types and type-checking   
* Source code to support unit  
* Source code to support pairs  
* Source code to support lists  

**Changed**  
* Tests for compiler  

**Known Bugs**  
None  

*Assignment 6*  
**Added**  
* Source code to support reference cells  
* Source code to support while loops
* Source code to support arrays

**Changed**  
* Tests for compiler  

**Known Bugs**  
None  

*Final Project*  
**Added**  
* Source code to support comments
* Source code to support repl mode

**Changed**  
Nothing

**Known Bugs**  
None  
