PARSER_SRC = LexMacchiato.hs ParMacchiato.hs SkelMacchiato.hs PrintMacchiato.hs AbsMacchiato.hs ErrM.hs

SRC = Main.hs Interpreter.hs DataStructures.hs InbuildFunctions.hs Utils.hs StaticTypeControl.hs

interpreter: $(SRC) $(PARSER_SRC)
	ghc Main.hs -o interpreter

clean:
	rm -f *.o *.hi interpreter
