### language
- [x] lower strings, chars
- [x] knot-tying
- [ ] pattern matching arms
    - [x] basic integer, char, atom patterns
    - [x] set patterns
    - [x] map patterns
    - [x] wildcards
    - [x] guards
    - [x] tuple patterns
    - [x] accepts?
    - [x] map structural equality, nested map keys
    - [x] list patterns, string patterns
    - [ ] type predicates
    - [ ] `match` ... `with`
- [ ] imports
- [ ] eval-aware Value formatter
- [ ] define all builtins
- [x] store an error list, pass that along
- [ ] core.k
- [ ] infixity
- [ ] runtime spans in eval, stack trace on eval panic
- [ ] fix map equality cycles
- [ ] Allow tuples/lists to be keys
- [ ] constant pool
- [ ] make maps hash maps rather than pair lists
- [ ] laziness
- [ ] string interpolation, string/char escapes
- [ ] `where`, `$` to end of line

### Core
- [ ] println
- [ ] basic funcs: map, filter, fold, length, range, reverse, sort, sum, min, max
- [ ] applicative algebra combinators `|`, `&`, `~`

### CLI
- [ ] actually make it exist, take in a file and eval the `main` symbol
- [ ] repl

### lang server
- [x] semantic tokens
    - [x] parameter coloring? or make non-func bindings more constant-looking?
- [ ] cache Analysis per URI, don't analyze twice per keystroke
- [ ] goto def
- [ ] autocomplete
- [ ] click-to-run (just eval the symbol)

### website
- [ ] wasm compiler