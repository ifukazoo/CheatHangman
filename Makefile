%:%.hs
	ghc -W $< -o $*

TARGETS	=\
		 Hangman\
		 Main\
		 Test\

all:$(TARGETS)

Hangman:	Hangman.hs
Main:		Main.hs Hangman.hs
Test:		Test.hs

.PHONY:	clean
clean:
	rm -rf $(TARGETS) *.o *.hi
