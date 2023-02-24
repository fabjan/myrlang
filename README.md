# Myrlang

A small subset of Erlang.

> What is this? A programming language for ants?

Myrlang is a tiny subset of Erlang. It can be embedded in a larger Erlang
system to replace the shell with something much less open to abuse. Expressions
are evaluated in an environment where all functions are provided explicitly by
the embedding system.

The Erlang subset is:
- only expressions, no forms, no compiler directives
- integers, atoms, strings
- lists, tuples, maps, binaries
- anonymous functions
- function application (limited to the environment)
- no pattern matching (but binding variables is allowed)
- no guards
- TBD: control flow (case? if? cond?)

## Build

```shell
$ rebar3 compile
```

## Test

```shell
$ rebar3 eunit
```

## Usage

TODO
