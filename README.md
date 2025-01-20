# Karta
Karta is a dynamically typed, lazy functional programming language with a focus on expressivity.

## Map Oriented
The name "Karta" derives from the fact that maps are a core data structure. Data structures in Karta are built off of immutable maps. Lists are maps, tuples are maps, sets are maps, and functions can be conceptually thought of as programmatic maps. 
```
;; This is a comment

my-map = {
  ;; Any value that begins with a `.` is an atomic value, and is basically just a named token
  .x = 5,
  .name = "Joseph",
  .age = 26
}

;; Lists are linked lists, and made up of map nodes with a head and tail field
;; The following lists are equivalent
my-list = [1, 2, 3]
my-list' = {
  .head = 1,
  .tail = {
    .head = 2,
    .tail = {
      .head = 3,
      .tail = {}
    }
  }
}

;; Sets map keys to `.t`
;; The following sets are equivalent
my-set = {1, 2, 3}
my-set' = {
  1 = .t,
  2 = .t,
  3 = .t
}

;; Tuples map their indices to values
;; The following tuples are equivalent
my-tuple = (1, 2, 3)
my-tuple' = {
  ;; Tuples are maps of indices to their values
  0 = 1,
  1 = 2,
  2 = 3,
}

;; Conceptually, the following map and function are equivalent:
square = {
  0 = 0,
  1 = 1,
  2 = 4,
  3 = 9,
  ;; ...
  ;; Karta doesn't support infinite maps, but just imagine this map is really huge and maps all integers
}
square' x = (* x x)

;; Getting values from maps and functions looks the same
four = square 2   ; map get
four' = square' 2 ; function call
```

Maps/functions can be unioned, intersected, and taken the difference of. This is useful for creating type predicates, as explained later on.
```
;; The union operator takes the key-value pairs of the right-hand-side, and if they don't exist, takes them from the left
;; The following two maps are equivalent
unioned = (|| {.x = 0, .y = 1} {.x = 2})
unioned' = {.x = 2, .y = 1}

;; The intersection operator creates a new functor with the common keys, and their values from the right-hand-side
;; The following two maps are equivalent
intersected = (&& {.x = 0, .y = 1} {.x = 2})
intersected' = {.x = 2}

;; The difference operator creates a new functor with the key-value pairs of the left-hand-side whose keys are not in the right
;; The following two maps are equivalent
differenced = (- {.x = 0, .y = 1} {.x = 2})
differenced = {.y = 1}

;; All the operators mentioned above work on any combination of maps or functions
even? n = (== (% n 2) 0)
positive? n = (> n 0)
positive-and-even? = (&& even? positive?)
positive-and-even-or-3 = (|| positive-and-even? {3})
```

## Type Predicates
Karta is a type-predicate based language. Types are made up of membership functors. These functors determine if a value is within the type. These type predicate membership functions can then be used for pattern matching.
```
;; An example type predicate, matches all even integers
even? (:n int?) = (== (% n 2) 0)

;; Can then use this while pattern matching to match on types
;; The `:predicate value` pattern matches if `predicate value` is truthy.
give-me-even (:even? _) = println "Thank you!"
give-me-even _          = println "lame"
```

## Open Multimethods
Karta uses open multimethods for polymorphism. This allows you to extend any function with your own custom pattern matches.
```
;; In core library:
length (:list? x) = ; length of a list
length (:map?  x) = ; length of the key set of the map

;; In your own custom file
import core

my-type = ; define your own type predicate

(core.length) (:my-type x) = ; extend length to work on your own custom type

;; In another file
print-length x = println (length x) ; this function works for any value that `length` accepts!
```