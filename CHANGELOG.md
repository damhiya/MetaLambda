# Revision history for MetaLambda

## Dev
Change
* match syntax `match xs with | [] -> true | y :: ys -> false end` -> `case xs of { [] -> true ; y :: ys -> false }`

## 0.3
Add
* `Subst` type (simultaneous substitution)

Change
* let-box syntax `let box[Ψ.U] = e1 in e2` -> `let box[U] = e1 in e2`
* clo syntax `U with (e1, ..., en)` -> `U with [e1/x1, ..., en/xn]`

Fix
* global substitution

## 0.2
Add
* bool, integer, product, and list type
* recursion using `fix`
* if-then-else syntax
* `Value` type

## 0.1
Simple CMTT implementation with:
* Type inference
* Evaluation
* Parser
* Pretty printer
* Basic REPL
