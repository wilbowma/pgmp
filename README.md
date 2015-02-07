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

Standard Usage
=====

To profile a module which uses a profile-guided meta-program:
```
raco pgmp --profile example.rkt
```

This will profile the code and generate `example.rkt.profile`.

To optimize and rerun the module:
```
> racket -t example.rkt
```

See [examples](../blob/master/rackpgmp/examples) for some example programs implemented
with or implementing profile-guided meta-programs.

Test and micrbenchmarks
======================

To run the test and microbenchmark suite:
```
make test
```

See [tests](../blob/master/rackpgmp/tests) for the test and microbenchmark suite.
