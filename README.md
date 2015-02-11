pgmp
====

Source code for [Profile-Guided Meta-Programming, PLDI 2015](https://www.williamjbowman.com/papers.html#pgmp).

Installing
==========

All Racket code runs under Racket 6.1.1.

To install the Racket package, run

```
raco pkg install git://github.com/bluephoenix47/pgmp/?path=rackpgmp
```

Quick Start
=====

Add `(require pgmp)` to your module, and use the provided API to make
compile-time decisions using profile information.

To profile a module which uses a profile-guided meta-program:
```
raco pgmp --profile example.rkt
```

This will profile the code and generate `example.rkt.profile`.

To optimize and rerun the module:
```
> racket -t example.rkt
```

For more information, run `raco docs pgmp`, `raco docs perflinty`, and see
[examples](../master/rackpgmp/examples).

Test and micro-benchmarks
======================

To run the test and micro-benchmark suite:
```
make test
```

See [tests](../master/rackpgmp/tests) for the test and micro-benchmark suite.
