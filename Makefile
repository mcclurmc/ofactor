BINARY=dist/build/ofactor/ofactor.byte
SRC=src/*.ml

OCAMLDEBUG=ledit ocamldebug
COMPILER_LIBS=$(shell ocamlfind query compiler-libs)
OBUILD_CONFIG=--verbose configure --enable-executable-bytecode --enable-executable-debugging
#OBUILD_BUILD=--debug+ build
OBUILD_BUILD=build

.DEFAULT: all
all: $(BINARY)

.PHONY: test debug clean configure

configure: dist/setup

dist/setup: ofactor.obuild
	obuild $(OBUILD_CONFIG)

%.byte: dist/setup $(SRC)
	obuild $(OBUILD_BUILD)
	cp dist/build/ofactor/*.cmt src/

test: dist/build/ofactor/ofactor.d.byte
	ocamlrun -b $^

debug: dist/build/ofactor/ofactor.d.byte
	$(OCAMLDEBUG) -I $(COMPILER_LIBS) -I dist/build/ofactor/ $^

clean:
	obuild clean
	rm -f src/*.cmt
