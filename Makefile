%:%.hs
	ghc -W $< -o $*

TARGETS	=\
		 Hangman\
		 Main\

all:$(TARGETS)

Hangman:	Hangman.hs
Main:		Main.hs

.PHONY:	clean
clean:
	rm -rf $(TARGETS) *.o *.hi
