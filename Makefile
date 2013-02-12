all:
	ghc --make site.hs

clean:
	rm -rf site
	rm -rf site.o
	rm -rf site.hi
