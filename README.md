# lambda-calculus

An implementation of the lambda calculus in Scala.

## Evaluation

Call-by-name is implemented in `Term.cbn`. Call-by-value is implemented in `Term.cbv`.

## REPL

The code includes a simple Cats Effect REPL.

### Example

```
$ sbt compile
$ sbt run
[info] running REPL
λ> (\x.x)(\x.x)
(λx.x)
```