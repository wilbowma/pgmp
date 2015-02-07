.PHONY: all paper.pdf clean test rackpgmp update remove

all:
	@echo "make paper.pdf  -- Build the paper."
	@echo "make test       -- Run the tests/microbenchmarks."
	@echo "make install    -- Install the rackpgmp package"
	@echo "make update     -- Update the rackpgmp package"
	@echo "make remove     -- Remove the rackpgmp package"

install:
	raco pkg install --link rackpgmp
	raco test rackpgmp/examples/*.rkt

update:
	raco pkg update --link rackpgmp
	raco test rackpgmp/examples/*.rkt

remove:
	raco pkg remove rackpgmp

test:
	cd rackpgmp; $(MAKE) $(MFLAGS) test

paper.pdf:
	cd paper; $(MAKE) $(MFLAGS) paper.pdf

clean:
	cd paper; $(MAKE) $(MFLAGS) clean
