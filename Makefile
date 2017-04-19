#PARSER = (
#LexMacchiato.hs
#ParMacchiato.hs
#SkelMacchiato.hs
#PrintMacchiato.hs
#AbsMacchiato.hs
#ErrM.hs
#)

interpreter: Main.hs #$(PARSER)
	ghc Main.hs -o interpreter

clean:
	rm *.o *.hi interpreter
