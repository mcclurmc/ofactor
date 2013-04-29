BINARY=dist/build/ofactor/ofactor.byte
SRC=src/*.ml

.DEFAULT: all
all: $(BINARY)

.PHONY: run clean configure

configure: dist/setup

dist/setup: ofactor.obuild
	obuild --verbose configure --enable-executable-bytecode --enable-executable-debugging

$(BINARY): dist/setup $(SRC)
#obuild --debug+ build
	obuild build
	cp dist/build/ofactor/*.cmt src/

test: dist/build/ofactor/ofactor.byte
	./dist/build/ofactor/ofactor.byte

clean:
	obuild clean
	rm -f src/*.cmt
