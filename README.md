# Jq in Haskell functional programming project

A clone of `jq` in Haskell.

## Build
```
> stack build
```

## Install

```
> stack install
```

this installs your executable to `~/.local/bin` by default (on *nix), make sure it's in $PATH

## Test

```
> stack test
```

You will need `jq` installed and available on `$PATH` to run `from-upstream` test suite.

## Use

```
> echo '{"this" : "that"}' | jq-clone '.this'
```

or

```
> echo '{"this" : "that"}' | stack run -- '.this'
```

## Test `jq` online

[jqplay.org](https://jqplay.org/)

## Docs

[stedolan.github.io/jq/manual](https://stedolan.github.io/jq/manual)
