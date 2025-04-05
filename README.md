# Lambda Calculus Parser & Evaluator

A comprehensive Haskell implementation for parsing, evaluating, and type-checking the untyped lambda calculus (ULC), with extensions toward System F (polymorphic lambda calculus).

## Features

- **Parser and Syntax Tree Builder** for lambda calculus expressions
- **Multiple Evaluation Strategies**:
  - Normal-order evaluation (call-by-name)
  - Applicative-order evaluation (call-by-value)
  - Big-step evaluation for complete program processing
- **Advanced Monadic Implementation**:
  - Context handling via State monad
  - Error handling with ExceptT transformer
  - IO capabilities for error reporting
- **Type System Components**:
  - Type unification algorithm
  - Type inference for polymorphic lambda calculus (System F)
  - Support for both strong and weak polymorphism

## Project Structure

```
.
├── src/
│   ├── Syntax/
│   │   ├── Expression.hs    # Core data types for lambda calculus
│   │   └── Grammar.hs       # Parser implementation
│   ├── Evaluation/
│   │   ├── Substitution.hs  # Free variable analysis and substitution rules
│   │   ├── Normal.hs        # Normal-order evaluation strategy
│   │   ├── Applicative.hs   # Applicative-order evaluation strategy  
│   │   └── Big.hs           # Big-step evaluation with global context
│   ├── Typing/
│   │   ├── Unification.hs   # Type unification algorithm
│   │   └── Inference.hs     # Type inference for System F
│   └── Main.hs              # Program entry point
├── prog.txt                  # Example input program
└── README.md                # This file
```

## Installation with Conda

Download and install the Haskell platform (http://www.haskell.org/platform/)

```bash
# Build the project
cabal update
cabal install HTF
```

## Usage

To parse, evaluate, and type-check a program:

```bash
runhaskell -isrc -itests InterpreterTest
```

## Implementation Details

### Parser

The parser transforms textual lambda calculus expressions into structured syntax trees, handling:
- Variables: `x`
- Functions (abstractions): `\x.expr`
- Applications: `(expr1 expr2)`
- Top-level definitions: `var=expr`

### Evaluation

#### Substitution Engine
- Manages free and bound variables
- Implements alpha-conversion to avoid variable capture
- Applies proper substitution according to lambda calculus rules

#### Evaluation Strategies
- **Normal-Order**: Evaluates leftmost, outermost redex first (lazy evaluation)
- **Applicative-Order**: Evaluates arguments before substitution (eager evaluation)

#### Advanced Monadic Features
- **State Management**: Uses `StateT` for tracking global context
- **Error Handling**: Implemented with `ExceptT` to catch undefined variables
- **IO Integration**: Reports errors to console through the IO monad

```haskell
-- Evolution of the Eval monad:
type Eval a = State Context a                              -- Initial implementation
type Eval a = StateT Context Identity a                    -- Equivalent form
type Eval a = ExceptT String (StateT Context Identity) a   -- With error handling
type Eval a = ExceptT String (StateT Context IO) a         -- With IO capabilities
```

### Type System

#### Type Unification
- Finds the most general unifier between two types
- Handles:
  - Free type variables
  - Function types
  - Occurrence checking to prevent infinite types

#### Type Inference
- Implements Hindley-Milner type inference algorithm
- Supports polymorphic types
- Distinguishes between:
  - Strong polymorphism (universally quantified type variables)
  - Weak polymorphism (free type variables)
- Uses lexical and dynamic contexts for proper scoping

## Example

Input program:
```
true=\x.\y.x 
false=\x.\y.y
 
not=\x.((x false) true)
(not true) 
(not false)
```

Type inference output:
```
(t6->(t7->t6))
(t9->(t10->t10))
 
(((t13->(t14->t14))->((t17->(t18->t17))->t20))->t20)
(t21->(t22->t22))
(t32->(t33->t32))
```

## Development Roadmap

- [x] Parser implementation
- [x] Substitution rules
- [x] Evaluation strategies (normal and applicative order)
- [x] Monadic context management
- [x] Error handling with ExceptT
- [x] IO capabilities
- [x] Type unification
- [x] Type inference for System F
- [ ] Add support for let-polymorphism
- [ ] Implement type-directed partial evaluation
