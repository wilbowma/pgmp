.PHONY: all paper.pdf clean test

all:
	@echo "make paper.pdf  -- Build the paper."
	@echo "make test       -- Run the tests/microbenchmarks."

test:
	cd rackpgmp; $(MAKE) $(MFLAGS) test

paper.pdf:
	cd paper; $(MAKE) $(MFLAGS) paper.pdf

clean:
	cd paper; $(MAKE) $(MFLAGS) clean
