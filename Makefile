.PHONY: all paper.pdf clean test rackpgmp update remove docs examples

all:
	@echo "make paper.pdf  -- Build the paper."
	@echo "make install    -- Install the rackpgmp package."
	@echo "make update     -- Update the rackpgmp package."
	@echo "make test       -- Run the tests/microbenchmarks."
	@echo "make examples   -- Run examples."
	@echo "make docs       -- Open the docs. rackpgmp must be installed."
	@echo "make remove     -- Remove the rackpgmp package."

install:
	cd rackpgmp; $(MAKE) $(MFLAGS) install

update:
	cd rackpgmp; $(MAKE) $(MFLAGS) update

docs:
	cd rackpgmp; $(MAKE) $(MFLAGS) docs

remove:
	cd rackpgmp; $(MAKE) $(MFLAGS) remove

test:
	cd rackpgmp; $(MAKE) $(MFLAGS) test

examples:
	cd rackpgmp; $(MAKE) $(MFLAGS) examples

paper.pdf:
	cd paper; $(MAKE) $(MFLAGS) paper.pdf

clean:
	cd paper; $(MAKE) $(MFLAGS) clean
