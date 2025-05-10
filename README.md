# MiniFunc - A Small Functional Programming Language

This is an interpreter for a small functional programming language with type inference, written in Haskell.

## Features

* **Typed Lambda Calculus**: Support for first-class functions, let bindings, and higher-order functions
* **Hindley-Milner Type Inference**: Type system based on the ML family of languages
* **Interactive REPL**: Evaluate expressions directly in a read-eval-print loop
* **Modular Design**: Cleanly separated components for parsing, type checking, and evaluation

## Language Structure

The language supports:

* Integer and boolean literals
* Variables 
* Arithmetic operations: `+`, `-`
* Comparison operator: `==`
* Conditional expressions: `if ... then ... else ...`
* Lambda expressions: `\x -> expr` or `λx -> expr`
* Function application: `f x`
* Let bindings: `let x = expr1 in expr2`
* Single-line comments start with "//" 

## Project Structure

* **AST.hs**: Abstract Syntax Tree definitions
* **Parser.hs**: Parsing using monadic parser combinators
* **TypeChecker.hs**: Hindley-Milner type inference engine
* **Evaluator.hs**: Expression evaluation and execution
* **Main.hs**: REPL and file processing

## Building and Running

### Prerequisites

* GHC (Glasgow Haskell Compiler)
* Cabal (Haskell build system)

### Setup

```bash
# Create a Cabal file
cat > minifunc.cabal <<EOF
name:                minifunc
version:             0.1.0.0
build-type:          Simple
cabal-version:       >=1.10

executable minifunc
  main-is:             Main.hs
  other-modules:       AST, Parser, TypeChecker, Evaluator
  build-depends:       base >=4.9,
                       containers >=0.5,
                       mtl >=2.2
  default-language:    Haskell2010
EOF

# Build the project
cabal build

# Run the REPL
cabal run

# Or run a file
cabal run -- factorial.mf
```

## Example Usage

### Interactive REPL

```
$ cabal run
MiniFunc Interpreter - A small functional language
Type :help for available commands
Type :quit to exit

> 1 + 2
Type: Int
Result: 3

> \x -> x + 1
Type: Int -> Int
Result: <function>

> let double = \x -> x + x in double 5
Type: Int
Result: 10

> :type \f -> \x -> f (f x)
Type: (t1 -> t1) -> t1 -> t1

> :quit
```

### Sample Program

```haskell
// Define a factorial function using recursion
let fact = \n -> if n == 0 then 1 else n * fact (n - 1) in fact 5

// Define a higher-order function that applies a function twice
let twice = \f -> \x -> f (f x) in
let add3 = \x -> x + 3 in twice add3 10
```

## Current Limitations

* Limited set of primitive operations
* No support for data structures like lists or records
* No modules or import system
* No user-defined data types

## Future Work

* Pattern matching
* Algebraic data types
* List comprehensions 
* More primitive operations and standard library
* Module system
