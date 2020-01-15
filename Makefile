PANDOC = pandoc
IFORMAT = markdown
TEMPLATE = resources/page.tmpl
LTEMPLATE = resources/page.latex
ETEMPLATE = resources/page.epubt
UNICODE_MAP = resources/unicodemapping.tex
FLAGS = --standalone \
				--toc \
				--toc-depth=2 \
				--highlight-style pygments \
				-c css/style.css \
				-c css/layout.css
GHC=ghc

HTML = tutorial.html
PDF = tutorial.pdf

# Check if sandbox exists. If it does, then use it instead.

all: $(HTML) $(PDF)

includes: includes.hs
	$(GHC) --make $< ; \

%.html: %.md includes
	./includes < $<  \
	| $(PANDOC) --template $(TEMPLATE) -s -f $(IFORMAT) -t html $(FLAGS) \
	| sed '/<extensions>/r extensions.html' > $@

%.epub: %.md includes
	(cat $(ETEMPLATE); ./includes < $<) \
	| $(PANDOC) -f $(IFORMAT) -t epub $(FLAGS) -o $@

%.pdf: %.md includes
	./includes < $< | $(PANDOC) -c -s -f $(IFORMAT) --template $(LTEMPLATE) --include-in-header $(UNICODE_MAP) --pdf-engine=xelatex $(FLAGS) -o $@

clean:
	-rm $(CHAPTERS) $(HTML) $(PDF)

# pandoc executable 'includes' is rather large
clean-all:
	rm -rf $(CHAPTERS) $(HTML) $(PDF) includes

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
