SRC=$(wildcard *.dot)
OUT=$(SRC:.dot=.png)

%.png : %.dot
	dot -Tpng -Gdpi=300 $< -o $@

all: $(OUT)

clean:
	rm -f $(OUT)
