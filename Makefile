PANDOC = pandoc
IFORMAT = markdown
FLAGS = --standalone --toc --toc-depth=2 --highlight-style pygments
TEMPLATE = page.tmpl
STYLE = css/style.css

HTML = tutorial.html

all: $(HTML)

includes: includes.hs
	ghc --make $<

%.html: %.md includes
	./includes < $< | $(PANDOC) -c $(STYLE) --template $(TEMPLATE) -s -f $(IFORMAT) -t html $(FLAGS) -o $@

%.epub: %.md includes
	./includes < $< | $(PANDOC) -f $(IFORMAT) -t epub $(FLAGS) -o $@

%.pdf: %.md includes
	./includes < $< | $(PANDOC) -c -s -f $(IFORMAT) --latex-engine=xelatex $(FLAGS) -o $@

clean:
	-rm $(CHAPTERS) $(HTML)
