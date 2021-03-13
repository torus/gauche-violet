TARGET=violet
CFLAGS=$(shell gauche-config -I) $(shell pkg-config --cflags libuv)
LDFLAGS=$(shell gauche-config -l) $(shell pkg-config --libs libuv)
LD_LIBRARY_PATH=$(shell gauche-config --sysarchdir)

RHEINGAU=./gauche-rheingau

SCRIPT=eg/random.scm

build: $(TARGET)

run: install
	rh1 install
#	LD_LIBRARY_PATH=$(LD_LIBRARY_PATH) ./$(TARGET) $(SCRIPT)
	LD_LIBRARY_PATH=$(LD_LIBRARY_PATH) nodemon -e scm --ignore gosh-modules/ --ignore gauche-rheingau/ --exec $(TARGET) $(SCRIPT)

## docker run --rm -p 2222:2222 -v$PWD:/code -w /code -t -i gauche-violet_gosh make debug
debug: $(TARGET) $(MAKIKI)
	gdb -ex run $(TARGET)

install: $(TARGET) $(RHEINGAU)
	$(MAKE) install

$(TARGET):
	$(MAKE) $@

$(RHEINGAU):
	$(MAKE) $@

clean:
	rm -rf *~ *.o $(TARGET) gosh-modules $(RHEINGAU) $(TARGET).dSYM
