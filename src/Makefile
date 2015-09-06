.SUFFIXES: .erl .beam .yrl

MODS := $(wildcard *.erl)
ERLC      = @echo "  ERLC\t$<"; /usr/local/bin/erlc
PDFLATEX  = @echo "  PDFLATEX\t$<"; /usr/texbin/pdflatex

%.beam: %.erl
	$(ERLC) -W $<

all: beams crypto.pdf

crypto.pdf: expand.tex
	latexmk -pdf crypto.tx

publish:
	make crypto.pdf
	cp crypto.pdf ../crypto_tutorial.pdf

expand.tex: crypto_latex.org
	erl -oldshell -s convert_org_to_latex test -s init stop

beams: ${MODS:%.erl=%.beam}

clean:
	rm -rf lecture_*.tex *.aux *.log *.toc *.out
	rm -rf erl_crash.dump *.beam expand.tex
	rm -rf expand.tex crypto.pdf


very_clean:
	rm -rf *.pdf 




