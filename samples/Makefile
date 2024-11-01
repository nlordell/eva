EVA = dune exec eva --no-print-directory --

.PHONY: all
all: $(patsubst %.evm,%.bin,$(wildcard *.evm))

%.bin: %.evm
	$(EVA) -x -o $@ $<

.PHONY: clean
clean:
	rm -f *.bin
