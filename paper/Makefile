SCRIBBLE=scribble
RM=rm

.PHONY: all clean

all: paper.pdf

paper.pdf: smaller.tex abstract.scrbl implementation.scrbl design.scrbl conclusion.scrbl example.scrbl case-studies.scrbl intro.scrbl main.scrbl related.scrbl defs.rkt bib.rkt results.scrbl bib.bib
	$(SCRIBBLE) ++style smaller.tex --pdf main.scrbl --dest-name paper.pdf

clean:
	$(RM) paper.pdf
