# Dwayne

A Haskell TUI app for GTD-like workflows using Org Mode

## Quick start

To install it system-wide use

```sh
cabal run
```

And then you can start using it

```sh
dwayne
```

## Profiling

```
cabal configure --enable-profiling
cabal run dwayne-hs -- +RTS -pj -RTS
```
