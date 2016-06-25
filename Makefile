PANDOC = pandoc
IFORMAT = markdown
FLAGS = --standalone --toc --toc-depth=2 --highlight-style pygments
TEMPLATE = resources/page.tmpl
LTEMPLATE = resources/page.latex
ETEMPLATE = resources/page.epubt
STYLE = css/style.css
GHC=ghc

HTML = tutorial.html

# Check if sandbox exists. If it does, then use it instead.

all: $(HTML)

includes: includes.hs
	$(GHC) --make $< ; \

%.html: %.md includes
	./includes < $<  \
	| $(PANDOC) -c $(STYLE) --template $(TEMPLATE) -s -f $(IFORMAT) -t html $(FLAGS) \
	| sed '/<extensions>/r extensions.html' > $@

%.epub: %.md includes
	(cat $(ETEMPLATE); ./includes < $<) \
	| $(PANDOC) -f $(IFORMAT) -t epub $(FLAGS) -o $@

%.pdf: %.md includes
	./includes < $< | $(PANDOC) -c -s -f $(IFORMAT) --template $(LTEMPLATE) --latex-engine=xelatex $(FLAGS) -o $@

clean:
	-rm $(CHAPTERS) $(HTML)
