.PHONY: all clean coverage distclean test

PBSRC := $(shell find . -name '*.proto')
PB := $(shell git ls-files '*_pb.ss')

all: $(PB)

$(PB): $(PBSRC) rebuild-protos protobuf/protoc
	./rebuild-protos

protobuf/conformance_test_runner protobuf/protoc: build-protobuf
	./build-protobuf

coverage: all protobuf/conformance_test_runner
	PROFILE=yes ./test-scheme

test: all protobuf/conformance_test_runner
	./test-scheme

clean:
	rm -f *.profile
	rm -f *.html
	rm -f *.{so,mo,sop}

distclean: clean
	rm -rf protobuf
