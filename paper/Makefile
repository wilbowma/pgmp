SCRIBBLE=scribble
RM=rm

.PHONY: all clean spell

all: paper.pdf

paper.pdf: acmformat.tex smaller.tex main.scrbl defs.rkt bib.rkt abstract.scrbl intro.scrbl example.scrbl design.scrbl implementation.scrbl beyondscheme.scrbl case-studies.scrbl related.scrbl conclusion.scrbl bib.bib study-case.scrbl study-datatype.scrbl study-virtual-call.scrbl
	$(SCRIBBLE) ++style acmformat.tex ++style smaller.tex --dest-name paper.pdf --pdf main.scrbl

spell:
	for i in *.scrbl; do aspell -c $i; done

check: spell paper.pdf
	pdffonts paper.pdf

# TODO: Relearn how to makefile. Or maybe make switch to raco or some
# TODO: such thing?
# main.scrbl: defs.rkt bib.rkt abstract.scrbl intro.scrbl example.scrbl design.scrbl implementation.scrbl beyondscheme.scrbl case-studies.scrbl related.scrbl conclusion.scrbl
#
# bib.rkt: bib.bib
#
# case-studies.scrbl: study-case.scrbl study-datatype.scrbl study-virtual-call.scrbl

clean:
	$(RM) paper.pdf
