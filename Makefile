ttt: main.hs
	ghc -Wall main.hs -o ttt

clean:
	rm -f main.o main.hi ttt
