exe = IrcBot

all: 
	ghc -isrc -threaded --make src/Main -main-is Main.main -o $(exe) -odir build -hidir build

clean:
	rm build/* $(exe)