ttt: main.hs
	ghc main.hs -o ttt

clean:
	rm -f main.o main.hi ttt
