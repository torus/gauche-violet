SCRIPT = random.scm
# SCRIPT = basic.scm

TARGET = violet

run:
	$(TARGET) $(SCRIPT)

build-local:
	$(MAKE) -C .. LIBDIR=../lib/ #LDFLAGS=-g

run-local: build-local
	../$(TARGET) $(SCRIPT)

debug:
	gdb ../$(TARGET)

run-local-nodemon: build-local
	nodemon -e scm --exec ../$(TARGET) $(SCRIPT)

install:
	rh1 install

clean:
	rm -rf *~ *.o $(TARGET) gosh-modules $(TARGET).dSYM
