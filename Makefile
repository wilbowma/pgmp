# Need my custom hacked version now.
SCRIBBLE=scribble

all: main.pdf

main.pdf: smaller.tex abstract.scrbl implementation.scrbl design.scrbl conclusion.scrbl example.scrbl case-studies.scrbl intro.scrbl main.scrbl related.scrbl defs.rkt bib.rkt results.scrbl bib.bib
	$(SCRIBBLE) ++style smaller.tex --pdf main.scrbl

