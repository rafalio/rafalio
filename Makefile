all:
	ghc --make site.hs

.PHONY: clean
clean:
	rm -rf site
	rm -rf site.o
	rm -rf site.hi
