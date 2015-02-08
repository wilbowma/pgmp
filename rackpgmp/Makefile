RACKET=racket
RACO=raco

.PHONY: all test examples

all:
	@echo "make test       -- Run the tests/microbenchmarks."
	@echo "make examples   -- Run examples."

test:
	cd tests; $(RACKET) -t run.rkt

examples:
	cd examples; $(RACO) test *.rkt
