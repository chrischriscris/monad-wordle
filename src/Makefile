all: 
	ghc --make -o wordle Main.hs

docs:
	haddock --html -o docs **.hs

clean:
	rm wordle
	find . -name "*.o" -type f -delete
	find . -name "*.hi" -type f -delete

cleandocs:
	rm -rf docs
	mkdir docs