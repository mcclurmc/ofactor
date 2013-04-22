
all: obuild-build

.PHONY: obuild-config obuild-build

obuild-config:
	obuild --verbose configure --enable-executable-bytecode --disable-executable-native

obuild-build: obuild-config
	obuild --debug+ build
