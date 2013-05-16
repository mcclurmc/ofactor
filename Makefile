BINARY=dist/build/ofactor/ofactor
BINARY_TEST=dist/build/ofactor/test-ofactor.d.byte
SRC=src/*.ml test/*.ml

OCAMLDEBUG=ledit ocamldebug
COMPILER_LIBS=$(shell ocamlfind query compiler-libs)
#OBUILD_BUILD=--debug+ build
OBUILD_BUILD=build
OBUILD_CONFIG=--verbose configure \
              --enable-tests \
              --enable-executable-debugging --enable-library-debugging \
              --enable-executable-bytecode --enable-library-bytecode

.DEFAULT: all
all: $(BINARY)

.PHONY: test debug clean configure

configure: dist/setup

dist/setup: ofactor.obuild
	obuild $(OBUILD_CONFIG)

%.byte: dist/setup $(SRC)
	obuild $(OBUILD_BUILD)
#	cp dist/build/ofactor/*.cmt src/

$(BINARY): dist/setup $(SRC)
	obuild $(OBUILD_BUILD)

$(BINARY_TEST): dist/setup $(SRC)
	obuild $(OBUILD_BUILD)

test: $(BINARY_TEST)
	ocamlrun -b $^

debug-test: dist/build/ofactor/test-ofactor
	$(OCAMLDEBUG) -I $(COMPILER_LIBS) -I dist/build/ofactor/ $^

run: $(BINARY)
	ocamlrun -b $^

clean:
	obuild clean
	rm -f src/*.cmt
