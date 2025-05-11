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
* Single-line comments starting with `//`

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

The interactive REPL in action:

![sample run](https://github.com/user-attachments/assets/24f3d381-f184-49a0-8cc2-4c13a014d30f)


### Sample Program

```haskell
// Define a factorial function using recursion and let binding
let sum = (\x -> (\y -> x + y)) in sum 10 10

// Define a higher-order function that applies a function twice
let twice = \f -> \x -> f (f x) in (let add3 = \x -> x + 3 in twice add3 10)

// Anonymous functions as arguments
let map = \f -> \xs -> if xs == 0 then 0 else f xs in map (\x -> x + 1) 5

// Testing boolean logic
if true then (if false then 0 else 1) else 2
```

Source file loaded using :load

![sample load](https://github.com/user-attachments/assets/301fe81f-450a-48d9-8c2c-94b53b10954d)


## Current Limitations
As the the focus of this project was basic features and type checking, many common language features have not been implemented:
* More complicated operations (increment, modulus, bitwise operations, etc)
* No support for characters and data structures like lists
* No user-defined data types
* No modules or import system

## Future Work

* Pattern matching
* Algebraic data types
* List comprehensions 
* More primitive operations and standard library
* Module system
* And more
