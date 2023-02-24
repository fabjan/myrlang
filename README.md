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

To run the automatic tests:

```shell
$ rebar3 eunit
```

To build and test the example REPL shell:

```shell
$  rebar3 escriptize
===> Verifying dependencies...
===> Analyzing applications...
===> Compiling myrlang
===> Building escript for myrlang...

$ _build/default/bin/myrlang

Welcome to Myrlang!

Shell commands:
    :help - show this help
    :quit - quit the REPL

Environment primitives:
    * + - < == div map not

> 4711 < 42
false
> map(fun (X) -> X*X end, [1, 2, 3, 4, 5])
[1,4,9,16,25]
> :quit

```

## Usage

TODO
