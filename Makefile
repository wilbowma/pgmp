# Need my custom hacked version now.
SCRIBBLE=~/workspace/racket-dev-goodies/plt-bin scribble

all: main.pdf

main.pdf: abstract.scrbl design.scrbl conclusion.scrbl examples.scrbl implementation.scrbl intro.scrbl main.scrbl related.scrbl defs.rkt bib.rkt results.scrbl
	$(SCRIBBLE) ++style smaller.tex --pdf main.scrbl

