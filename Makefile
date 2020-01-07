PANDOC = pandoc
IFORMAT = markdown
GHC = ghc

HTEMPLATE = resources/page.tmpl
LTEMPLATE = resources/page.latex
ETEMPLATE = resources/page.epubt

FLAGS = --standalone --toc --toc-depth=2 --highlight-style pygments
LFLAGS = --top-level-division=chapter -V documentclass=book
HFLAGS = -c css/style.css -c css/layout.css
EFLAGS = 

HTML = tutorial.html
EPUB = tutorial.epub
PDF = tutorial.pdf

all: $(HTML) $(EPUB) $(PDF)
html: $(HTML)
pdf: $(PDF)
epub: $(epub)

includes: includes.hs
	$(GHC) --make $<

%.html: %.md includes
	./includes < $<  \
	| $(PANDOC) --template $(HTEMPLATE) -s -f $(IFORMAT) -t html $(FLAGS) $(HFLAGS) \
	| sed '/<extensions>/r extensions.html' > $@

%.epub: %.md includes
	(cat $(ETEMPLATE); ./includes < $<) \
	| $(PANDOC) -f $(IFORMAT) -t epub $(FLAGS) $(EFLAGS) -o $@

%.pdf: %.md includes
	./includes < $< | $(PANDOC) -c -s -f $(IFORMAT) --template $(LTEMPLATE) --pdf-engine=xelatex $(FLAGS) $(LFLAGS) -o $@

clean:
	-rm -f $(CHAPTERS) $(HTML) $(PDF) $(EPUB)

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
