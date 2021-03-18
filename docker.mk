TARGET=violet
CFLAGS=$(shell gauche-config -I) $(shell pkg-config --cflags libuv)
LDFLAGS=$(shell gauche-config -l) $(shell pkg-config --libs libuv)
LD_LIBRARY_PATH=$(shell gauche-config --sysarchdir)

SCRIPT_DIR = eg
SCRIPT = random.scm
# SCRIPT = basic.scm

run:
	$(TARGET) $(SCRIPT_DIR)/$(SCRIPT)

build-local:
	$(MAKE) LIBDIR=../lib/ #LDFLAGS=-g

run-local: build-local
	cd $(SCRIPT_DIR) && ../$(TARGET) $(SCRIPT)

debug:
	cd $(SCRIPT_DIR) && gdb ../$(TARGET)

run-local-nodemon: build-local
	cd $(SCRIPT_DIR) && nodemon -e scm --exec ../$(TARGET) $(SCRIPT)

install:
	$(MAKE) install
	rh1 install

clean:
	rm -rf *~ *.o $(TARGET) gosh-modules $(TARGET).dSYM
