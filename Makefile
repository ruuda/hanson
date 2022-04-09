bin/hanson: $(shell find src)
	mkdir -p bin build
	ghc -Wall -Wincomplete-record-updates -Wincomplete-uni-patterns -o bin/hanson -outputdir build src/Main.hs

run: bin/hanson
	bin/hanson

.PHONY: run
