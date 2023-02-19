# Myrlang

A small subset of Erlang.

> What is this? A programming language for ants?

Myrlang is a tiny subset of Erlang. It can be embedded in a larger Erlang
system to replace the shell with something much less open to abuse. Expressions
are evaluated in an environment where all functions ae provided explicitly by
the embedding system.

The Erlang subset is:
- only expressions, no forms, no compiler directives
- integers, atoms, strings
- no sequences (no comma)
- no match expressions
- no guards
- function application (limited to the environment)
- some operators
  - arithmetic: +, -, *, /, rem, div
  - comparisons: =:=, =<, <
  - (eager) boolean operations: and, or
- lists, tuples, maps, binaries
- TBD: control flow
- TBD: anonymous functions
- TODO: bind variables in the environment (not functions though)

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
