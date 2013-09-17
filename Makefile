all: main.pdf

main.pdf: abstract.scrbl api.scrbl conclusion.scrbl examples.scrbl implementation.scrbl intro.scrbl main.scrbl related.scrbl bib.bib defs.rkt bib.rkt
	scribble --pdf main.scrbl

