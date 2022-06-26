HC = ghc
CFLAGS = --make

all: wordle

wordle: wordle.hs Utils/Checkers.hs
	$(HC) $(CFLAGS) wordle.hs

clean:
	rm *.o *.hi wordle Utils/*.o Utils/*.hi