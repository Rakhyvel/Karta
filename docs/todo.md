### language
- [x] lower strings, chars
- [x] knot-tying
- [ ] pattern matching arms
    - [x] basic integer, char, atom patterns
    - [x] set patterns
    - [x] map patterns
    - [x] wildcards
    - [x] guards
    - [ ] tuple patterns
    - [ ] accepts?
    - [ ] map structural equality, nested map keys
    - [ ] list patterns, string patterns
    - [ ] `match` ... `with`
    - [ ] type predicates
- [ ] imports
- [ ] eval-aware Value formatter
- [ ] define all builtins
- [ ] infixity
- [x] store an error list, pass that along
- [ ] core.k
- [ ] runtime spans in eval, stack trace on eval panic
- [ ] laziness
- [ ] string interpolation, string/char escapes
- [ ] `where`, `$` to end of line
- [ ] `when` for guards

### Core
- [ ] applicative algebra combinators `|`, `&`, `~`

### CLI
- [ ] actually make it exist, take in a file and eval the `main` symbol
- [ ] repl

### lang server
- [x] semantic tokens
    - [x] parameter coloring? or make non-func bindings more constant-looking?
- [ ] cache Analysis per URI, don't analyze twice per keystroke
- [ ] goto def
- [ ] click-to-run (just eval the symbol)