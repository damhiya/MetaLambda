# MetaLambda
Implementation of Contextual Modal Type Theory[Nanevski et al. 2008]

## Example
```
>> fn (x : base) -> x
fn (x : base) -> x
base -> base
fn (x : base) -> x
```
```
>> let box[U] = box[f : base -> base . fn (x : base) -> (f (f (f x)))] in box[f : base -> base. U with [U with [f/f]/f]]
let box[U] = box[f : base -> base . fn (x : base) -> f (f (f x))] in box[f : base -> base . U with [U with [f / f] / f]]
[f : base -> base |- base -> base]
box[f : base -> base . fn (x : base) -> (fn (x : base) -> f (f (f x))) ((fn (x : base) -> f (f (f x))) ((fn (x : base) -> f (f (f x))) x))]
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
