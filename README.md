# jay
An OCaml library for creating Earley parsers.

## Why Earley parsers?
In comparison to traditional LALR parsers, Earley parsers are:

- Really easy to write; jay can build them directly from ABNF specifications
- Work for most grammars regardless of left/right-recursiveness and ambiguity
- Scannerless (useful in cases where it is difficult to write a separate lexer)
- Easy to make incremental
- Easy(ish) to understand
