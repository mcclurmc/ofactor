BINARY=dist/build/ofactor/ofactor.byte
SRC=src/*.ml

OCAMLDEBUG=ledit ocamldebug
COMPILER_LIBS=$(shell ocamlfind query compiler-libs)

.DEFAULT: all
all: $(BINARY)

.PHONY: test debug clean configure

configure: dist/setup

dist/setup: ofactor.obuild
	obuild --verbose configure --enable-executable-bytecode --enable-executable-debugging

$(BINARY): dist/setup $(SRC)
#obuild --debug+ build
	obuild build
	cp dist/build/ofactor/*.cmt src/

test: dist/build/ofactor/ofactor.d.byte
	ocamlrun -b $^

debug: dist/build/ofactor/ofactor.d.byte
	$(OCAMLDEBUG) -I $(COMPILER_LIBS) $^

clean:
	obuild clean
	rm -f src/*.cmt
