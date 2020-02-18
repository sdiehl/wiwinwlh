PANDOC = pandoc
IFORMAT = markdown+raw_tex+raw_attribute+auto_identifiers+implicit_header_references
GHC = ghc

INPUT = tutorial.md

HTEMPLATE = resources/template.html
LTEMPLATE = resources/template.tex
ETEMPLATE = resources/template.epub


FLAGS = --standalone --toc --toc-depth=2 --highlight-style tango
LFLAGS = --top-level-division=chapter
PFLAGS = -V colorlinks
HFLAGS = -c css/style.css -c css/layout.css
DFLAGS =
EFLAGS = 

# Targets
HTML = tutorial.html
EPUB = tutorial.epub
PPDF = tutorial_print.pdf
WPDF = tutorial.pdf
TEX = tutorial.tex
DOCX = tutorial.tex

# LaTeX
COVER = resources/cover.tex
BACK = resources/back.tex
UNICODE_MAP = resources/unicodemapping.tex

all: $(PDF) $(HTML) $(EPUB)
html: $(HTML)
docx: $(DOCX)
epub: $(epub)
pdf: $(PPDF)

# Code snippet preprocessor
includes: includes.hs
	$(GHC) --make $<

# .html Target
%.html: %.md includes
	./includes < $<  \
	| $(PANDOC) --template $(HTEMPLATE) -s -f $(IFORMAT) -t html $(FLAGS) $(HFLAGS) \
	| sed '/<extensions>/r extensions.html' \
	| sed '/<copyright>/r resources/copyright.html' > $@

# .docx Target
%.docx: %.md includes
	./includes < $<  \
	| $(PANDOC) --template $(HTEMPLATE) -s -f $(IFORMAT) -t docx $(FLAGS) $(DFLAGS) > $@

# .epub Target
%.epub: %.md includes
	(cat $(ETEMPLATE); ./includes < $<) \
	| $(PANDOC) -f $(IFORMAT) -t epub $(FLAGS) $(EFLAGS) -o $@

# .tex Target
%.tex: %.md includes $(COVER) $(BACK)
	./includes < $< \
	| $(PANDOC) -c -s -f $(IFORMAT) --template $(LTEMPLATE) --include-in-header $(UNICODE_MAP) --pdf-engine=xelatex $(FLAGS) $(LFLAGS) -o $@

# .pdf Target for Print
%.pdf: $(INPUT) includes $(COVER) $(BACK)
	./includes < $< \
	| $(PANDOC) -c -s -f $(IFORMAT) --template $(LTEMPLATE) --include-in-header $(UNICODE_MAP) --pdf-engine=xelatex $(FLAGS) $(LFLAGS) -o $(PPDF)

# dirty hack
# .pdf Target for Web
webpdf: $(INPUT) includes $(COVER) $(BACK)
	./includes < $< \
	| $(PANDOC) -c -s -f $(IFORMAT) --template $(LTEMPLATE) --include-in-header $(UNICODE_MAP) --pdf-engine=xelatex $(FLAGS) $(LFLAGS) $(PFLAGS) -o $(WPDF)

links:
	brok tutorial.md

clean:
	-rm -f $(CHAPTERS) $(HTML) $(PDF) $(EPUB) $(TEX)

# pandoc executable 'includes' is rather large
clean-all:
	rm -rf $(CHAPTERS) $(HTML) includes

# NIX BUILD
# Enter nix shell with 'make run-shell' first (then 'make all')

.PHONY : run-shell
run-shell : shell.nix
ifndef NIX_GHC
	nix-shell
else
	$(error Already in GHC shell!)
endif

shell.nix : wiwinwlh.cabal
	cabal2nix --shell  . > shell.nix
