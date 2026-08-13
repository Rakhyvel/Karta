# Karta
Karta is a dynamically typed, map-oriented functional programming language.

All values in Karta are either atomic or applicable.

## Maps

Maps map keys to values. Map lookup is done via application.

```
me = {.name = "Joseph", .age = 27}
me.name     ; "Joseph"
me.age      ; 27
```

Sets are maps whose values are `.t`.
```
my-set = {.apple, .banana, .car}
my-set' = {.apple = .t, .banana = .t, .car = .t}
```

Tuples are maps keyed by index.
```
my-tuple = (24, 56, 67, 89)
my-tuple' = {0 = 24, 1 = 56, 2 = 67, 3 = 89}
```

Lists are made up of cons cells.
```
my-list = [1, 2, 3]
my-list' = {.head = 1, .tail = {.head = 2, .tail = {.head = 3, .tail = {}}}}
```

Strings are lists of UTF-8 chars.
```
my-name = "Joe"
my-name' = ['J', 'o', 'e']
my-name'' = {.head = 'J', .tail = {.head = 'o', .tail = {.head = 'e', .tail = {}}}}
```

## Functions
Functions take one argument, and are defined by pattern matching clauses tried in-order.
```
len [] = 0
len [_, ..rest] = 1 + (len rest)
```
## Applicative Algebra
Functions and maps can be unioned, intersected, and negated.
```
cool-number = {42, 67, 69, 420}
even? (n: int?) = n % 2 == 0

even-and-cool-number? = cool-number & even?
even-or-cool-number? = cool-number | even?
lame-number? = ~cool-number
```