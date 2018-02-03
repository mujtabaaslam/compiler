# A Simple Compiler
## Mujtaba Aslam
A simple compiler which a software system that translates programs from one form to another.

**CLI**
cli takes an amount of arguments and prints them out in order, one in each line. It also supports two command flags:
-length -- prints out the lengths of the arguments instead of the arguments themselves.
-help -- prints out a usage message for cli.
--help -- prints out a usage message for cli
.
**Setup Instructions**
CLI uses the core library for OCaml. The ocamlc compiler is used to build this program.

**Build Instructions**
To build the program simply run `make` in the root directory. To remove the compiled files simply run `make clean`.

**Execution Instructions**
Run the cli command using `./cli [flag] [args]`.

**Hooks**
Hooks can be installed to keep commits clean and correct. To install hooks run the `./makehook.sh` command in the githooks directory.

**Change Log**
