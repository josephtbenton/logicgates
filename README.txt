   __             _       _____     __
  / /  ___  ___ _(_)___  / ___/__ _/ /____ ___
 / /__/ _ \/ _ '/ / __/ / (_ / _ '/ __/ -_|_-<
/____/\___/\_, /_/\__/  \___/\_,_/\__/\__/___/
       /___/

An imperative, domain-specific programming language
by Joseph Benton (josephtbenton@gmail.com)

LogicGates is a simple language that allows for basic
boolean algebra computations.



------------------------------------------
------------ Syntax -------------------
------------------------------------

<prog> ::= <stmt> [ ';' <stmt> ]*

<stmt> ::=
  | <var> '=' <expr>
  | 'input' <var>
  | 'output' <expr>

<expr> ::=
  | 'False' | 'True'
  | <var>
  | <uop> <expr>
  | <expr> <bop> <expr>

<uop> ::= 'not' | '~'

<bop> ::= '&' | '|' | 'and' | 'or' | 'xor' | 'nor' | 'nand' |


------------------------------------------
------------ Installing ---------------
------------------------------------

To use LogicGates, you must have ghc and Parsec installed.
ghc:    https://www.haskell.org/ghc/download_ghc_7_6_1
Parsec: https://hackage.haskell.org/package/parsec

run `ghc --make logic.hs`

This command results in an executable, `logic`.


------------------------------------------
------------ Usage --------------------
------------------------------------

Note:
Statements must be separated by a semicolon.
The last statement must not end in a semicolon

--------- IO ----------------------
'input' <var>   -> take input from the console
                   Valid truthy inputs: True, T, 1
                   Valid falsey inputs: False, F, 0
'output' <expr> -> output the result of an expression to the console

-------- Boolean Operators ---------
'and', '&'      -> logical 'and' infix operator
'or', '|'       -> logical 'or' infix operator
'not', '~'      -> inverse unary operator
'xor'           -> logical 'xor' infix operator
'nor'           -> logical 'nor' infix operator
'nand'          -> logical 'nand' infix operator

Note: The 'xor', 'nor', 'nand' operators are desugared to AST's consisting of
      'and', 'or', 'not' prior to interpretation.

-------- Variable Assignment --------
<var> = <expr>  -> assigns an expression to a variable


-------- Execution ------------------
Example: running a program
run `./logic test.txt`

Example: runing a program with verbose output
run `./logic test.txt -v`

When executing, if an input is encountered, computation will halt until the user
inputs a truthy or falsey value and presses return.

When execution is halted, LogicGates prints any output from the program, as well
as the value of all variables. This is useful for seeing what expressions were
desugared to.
