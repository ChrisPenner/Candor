# Candor

A toy language I'm writing to learn more about parsing, compiling, typechecking
and type inference. It's a simple lisp-like expression-based language.

```lisp
;; It uses prefix notation for function application
;; Variadic functions are not yet supported
(+ 1 2)
;; 3

(++ "hello, " "world!")
;; hello, world!

;; You can define lambdas using the `def` keyword; `:` denotes symbols in a binding position
((def [:num] (* num num)) 5)
;; 25

;; `=` creates a binding between a symbol and an expression.
;; You then call the binding as though it's a function to bring its bindings into scope:
(
    (= :square (def [:num] (* num num))) 
    (square 5)
)
;; 25

;; `merge` a list of bindings into a set of bindings
(
    (merge [ (= :x 10)
             (= :y 42)
             (= :square (def [:num] (* num num)))])
    (+ (square x) y)
    )
)
;; 142
```

Currently unsupported:
- [x]  ~~Recursion~~
- [ ]  Type inference
- [ ]  IO
- [ ]  Datatypes

Like seriously, don't even try and use this thing.
