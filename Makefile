# Need my custom hacked version now.
SCRIBBLE=scribble

all: main.pdf

main.pdf: smaller.tex abstract.scrbl design.scrbl conclusion.scrbl examples.scrbl implementation.scrbl intro.scrbl main.scrbl related.scrbl defs.rkt bib.rkt results.scrbl bib.bib
	$(SCRIBBLE) ++style smaller.tex --pdf main.scrbl

