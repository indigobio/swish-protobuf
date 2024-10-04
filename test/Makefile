.PHONY: all clean coverage distclean test

PBSRC := $(shell find . -name '*.proto')
PB := $(shell git ls-files '*_pb.ss')

all: $(PB)

$(PB): $(PBSRC) rebuild-protos
	./rebuild-protos

protobuf/bin/conformance_test_runner: build-protobuf
	./build-protobuf

coverage: all protobuf/bin/conformance_test_runner
	PROFILE=yes ./test-scheme

test: all protobuf/bin/conformance_test_runner
	./test-scheme

clean:
	rm -f *.profile
	rm -f *.html
	rm -f *.{so,mo,sop}

distclean: clean
	rm -rf protobuf
