BINARY=dist/build/ofactor/ofactor.byte

all: $(BINARY)

.PHONY: run clean

dist: ofactor.obuild
	obuild --verbose configure --enable-executable-bytecode

$(BINARY): dist
	obuild --debug+ build
	cp dist/build/ofactor/*.cmt src/

run: dist/build/ofactor/ofactor.byte
	./dist/build/ofactor/ofactor.byte

clean:
	obuild clean
	rm -f src/*.cmt
