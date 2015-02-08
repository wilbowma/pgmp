.PHONY: all test examples

all:
	@echo "make install    -- Install the rackpgmp package."
	@echo "make update     -- Update the rackpgmp package."
	@echo "make test       -- Run the tests/microbenchmarks."
	@echo "make examples   -- Run examples."
	@echo "make docs       -- Open the docs. Package must be installed."
	@echo "make remove     -- Remove the rackpgmp package."

install:
	raco pkg install --link
	$(MAKE) $(MFLAGS) examples

update:
	raco pkg update --link ../rackpgmp
	$(MAKE) $(MFLAGS) examples

remove:
	raco pkg remove rackpgmp

test:
	cd tests; racket -t run.rkt

docs:
	raco docs pgmp
	raco docs perflinty

examples:
	cd examples; raco test *.rkt
