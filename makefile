exe = IrcBot

all: 
	ghc -isrc -threaded --make src/IrcMain -main-is IrcMain.main -o $(exe) -odir build -hidir build

clean:
	rm build/* $(exe)