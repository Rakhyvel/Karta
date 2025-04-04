;;;                           ;;;
;;; Equality and orderability ;;;
;;;                           ;;;

== x y = @eql(x, y)
!= x y = @neq(x, y)
< x y = @lsr(x, y)
<= x y = @lte(x, y)
> x y = @gtr(x, y)
>= x y = @gte(x, y)

;; @is
;; @isnt

;;;                      ;;;
;;; Arithmetic Operators ;;;
;;;                      ;;;

+ x y = @add(x, y)
- x y = @sub(x, y)
* x y = @mul(x, y)
/ x y = @div(x, y)
% x y = @mod(x, y)
&& x y = @and(x, y)
|| x y = @or(x, y)
not x = @not(x)

;; // x y
;; **
;; ^^ ;; xor I think?
;; >>
;; <<
;; neg
;; ~
;; divmod
;; abs
;; round
;; trunc
;; floor
;; ceil

;;;                        ;;;
;;; Containers & Iterators ;;;
;;;                        ;;;

length [] = 0
length [x..xs] = + (length xs) 1

fib 0 = 0
; fib does not exist. create a lambda that takes in the arity number of arguments, and has an `if-elif-else` chain:
;   \arg0 -> if (== arg0 0) then 0 else (panic! "function `fib` is not total!")
fib 1 = 1
; fib does exist. Modify the lambda expression's if statement:
;   \arg0 -> ... elif (== arg0 1) then 1 ...
fib n = ...
; fib does exist. Modify the lambda expression's if statement:
;   \arg0 -> .. elif (.t) then ...

; arity MUST match!
; fib 0 4 = 5 ;=> ERROR!

; constants (defs with arity 0) don't need lambdas
my-constant = 5
; my-constant does not exist. arity is 0. just create a def for it

;; contains
;; next
;; reverse 
;; get
;; set
;; append
;; prepend
;; sort
;; sort-by
;; all
;; any
;; map
;; foldl
;; foldr
;; filter
;; maximum
;; minimum
;; maximum-by
;; minimum-by
;; count
;; count-by
;; first
;; last

;;;                           ;;;
;;; Functions and combinators ;;;
;;;                           ;;;

;; curry
;; uncurry
;; on
;; @keys
;; square
;; choose
;; max
;; min

;;;                              ;;;
;;; Type conversions and strings ;;;
;;;                              ;;;

;; string<-
;; tuple<-
;; bool<-
;; int<-
;; float<-
;; seq<-
;; char<-
;; set<-
;; hex<-
;; dec<-
;; oct<-
;; bin<-
;; split
;; collapse - collapses a tuple of characters to a string
;; slice - takes a tuple and a min and max and returns a new tuple

;;;                                  ;;;
;;; Type predicates and polymorphism ;;;
;;;                                  ;;;

;; atom?
;; map?
;; function?
;; int?
;; bool?
;; float?
;; rational?
;; list?
;; string?
;; tuple?
;; accepts?
;; non-decreasing?
;; sorted?
;; begins-with?
;; empty?
;; impls?

;;;                  ;;;
;;; Input and output ;;;
;;;                  ;;;

;; print
;; println
;; getln
;; open
;; close
;; write
;; read
