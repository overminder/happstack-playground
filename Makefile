main-static : Main.hs App.hs
	ghc Main -static -optl-static -optl-pthread -O

clean :
	rm *.o Main
