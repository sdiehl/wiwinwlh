PANDOC = pandoc
IFORMAT = markdown
FLAGS = --standalone --toc --toc-depth=1 --highlight-style pygments
TEMPLATE = page.tmpl
STYLE = css/style.css

HTML = slideshow.html

all: $(HTML)

includes: includes.hs
	ghc --make $<

%.html: %.md includes
	# $(PANDOC) -c $(STYLE) --template $(TEMPLATE) -s -f $(IFORMAT) -t html $(FLAGS) -o $@ $<
	./includes < $< | $(PANDOC) -c $(STYLE) --template $(TEMPLATE) -s -f $(IFORMAT) -t html $(FLAGS) -o $@

clean:
	-rm $(CHAPTERS) $(HTML)
