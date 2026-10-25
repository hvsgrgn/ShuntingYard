# Shunting Yard

A Haskell take on Dijkstra's **shunting-yard algorithm**. It parses and evaluates math expressions written in ordinary infix notation, like `1 + 2 * 3`. The project is built up in stages, from plain tokenizing to full evaluation with operator precedence, functions and error reporting.

## Features

- **Tokenizer** that turns a string into literals, operators and brackets
- **Binary operators** `+ - * / ^`, with precedence and associativity (`^` is right-associative)
- **Brackets**, including nested ones
- **Functions:** `sin`, `cos`, `log`, `exp`, `sqrt`, `round`
- **Error handling** with `Either` instead of `Maybe`. Errors say what went wrong:
  - `ParseError`: a token couldn't be read
  - `NoOpeningParen` / `NoClosingParen`: brackets don't match
  - `OperatorOrClosingParenExpected` / `LiteralOrOpeningParenExpected`: tokens are in the wrong order

## Running it

You need [GHC](https://www.haskell.org/ghcup/). Load the file in GHCi:

```
ghci ShuntingYard.hs
```

Then try the evaluators. **Separate every token with spaces**, including the brackets:

```haskell
ghci> syComplete "sqrt ( 3 + 6 ) * 2"
Right ([6.0],[])

ghci> syComplete "round 2.6 + 1"
Right ([4.0],[])

ghci> syEvalPrecedence "2 ^ 3 ^ 2"     -- right-associative: 2 ^ (3 ^ 2)
Just ([512.0],[])

ghci> syComplete "( 1 + 2"
Left NoClosingParen

ghci> syComplete "1 + * 2"
Left LiteralOrOpeningParenExpected
```

The result is the stack of computed values and the remaining operator stack. After a full evaluation, the operator stack is empty.

## Stages

Each stage is a separate entry-point function:

| Function           | What it does                                         |
|--------------------|------------------------------------------------------|
| `syNoEval`         | Tokenizes only and returns the value and operator stacks |
| `syEvalBasic`      | Evaluates brackets, without operator precedence      |
| `syEvalPrecedence` | Adds operator precedence and associativity           |
| `syFun`            | Adds the functions (`sin`, `sqrt`, …)                |
| `sySafe`           | Error reporting with `Either ShuntingYardError`      |
| `syComplete`       | Everything above combined                            |

## About

Built as a university Haskell assignment. Comments in the code are in Hungarian. The `basicInstances` and `eqError` definitions are required by the course's automatic tester.
