# -*- Makefile -*-

### Assumptions:
### 1. If you run `make all`, you want to compile every TEX file in the current
###    directory
### 2. After compilation, you want to move every PDF file to the parent
###    directory

## Find all TEX files:
TEXS := $(wildcard *.tex)

## Strip the file extension off the located TEX files:
NAMES := $(TEXS:%.tex=%)

all: ${NAMES}

%: %.tex
	latexmk -pdf $@.tex

clean:
	mv *.pdf ../
	rm -f *.aux *.log *.out *.nav *.snm *.toc *.vrb *.bbl *.blg *.fdb_latexmk *.fls
