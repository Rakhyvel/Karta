;;;                           ;;;
;;; Equality and orderability ;;;
;;;                           ;;;

== x = {x}
!= x = ~{x}
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
