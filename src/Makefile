HC = ghc
CFLAGS = --make

all: wordle

wordle: wordle.hs Utils/Checkers.hs Utils/IOHelpers.hs Utils/Minimaxer.hs
	$(HC) $(CFLAGS) wordle.hs

clean:
	rm *.o *.hi wordle Utils/*.o Utils/*.hi