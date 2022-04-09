bin/hanson: $(shell find src)
	mkdir -p bin
	mkdir -p build
	ghc -Wall -o bin/hanson -outputdir build src/Main.hs

