TARGET=violet
CFLAGS=$(shell gauche-config -I)
LIBPATH=$(shell gauche-config -L)
LFLAGS=-luv $(LIBPATH) -lgauche-0.97
LD_LIBRARY_PATH=$(shell gauche-config --sysarchdir)

RHEINGAU=./gauche-rheingau

SCRIPT=eg/random.scm

build: $(TARGET)

run: $(TARGET) $(RHEINGAU)
	$(RHEINGAU)/rh1 install
#	LD_LIBRARY_PATH=$(LD_LIBRARY_PATH) ./$(TARGET) $(SCRIPT)
	LD_LIBRARY_PATH=$(LD_LIBRARY_PATH) nodemon -e scm --ignore gosh-modules/ --ignore gauche-rheingau/ --exec ./$(TARGET) $(SCRIPT)

## docker run --rm -p 2222:2222 -v$PWD:/code -w /code -t -i gauche-violet_gosh make debug
debug: $(TARGET) $(MAKIKI)
	gdb -ex run $(TARGET)

$(TARGET): main.c
	$(CC) -g -I/usr/local/include -o $(TARGET) main.c $(CFLAGS) $(LFLAGS)

$(RHEINGAU):
	git clone https://github.com/torus/gauche-rheingau.git $(RHEINGAU)

clean:
	rm -rf *~ *.o $(TARGET) gosh-modules $(RHEINGAU) $(TARGET).dSYM
