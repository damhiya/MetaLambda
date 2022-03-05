# MetaLambda
Implementation of Contextual Modal Type Theory[Nanevski et al. 2008]

## Example
```
>> fn (x : base) -> x
fn (x_0 : base) -> x_0
(base -> base)
fn (x_0 : base) -> x_0
```
```
>> let box[f.U] = box[f : base -> base . fn (x : base) -> (f (f (f x)))] in box[f : base -> base. U with (U with (f))]
let box[f_0 . U_0] = box[f_0 : (base -> base) . fn (x_0 : base) -> (f_0 (f_0 (f_0 x_0)))] in box[f_0 : (base -> base) . U_0 with ( U_0 with ( f_0 ) )]
[f_0 : (base -> base) |- (base -> base)]
box[f_0 : (base -> base) . fn (x_1 : base) -> (fn (x_0 : base) -> (f_0 (f_0 (f_0 x_0))) (fn (x_0 : base) -> (f_0 (f_0 (f_0 x_0))) (fn (x_0 : base) -> (f_0 (f_0 (f_0 x_0))) x_1)))]
```

## Build
### using cabal
```sh
cabal build
cabal run
```

### using nix
You need nix flake feature
```sh
nix build .
```

## Code formatting
```sh
stylish-haskell -ir src
```
