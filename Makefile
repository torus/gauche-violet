TARGET=violet
CFLAGS=$(shell gauche-config -I)
LIBPATH=$(shell gauche-config -L)
LFLAGS=-luv $(LIBPATH) -lgauche-0.97
LD_LIBRARY_PATH=$(shell gauche-config --sysarchdir)

KAHEKA=gosh-modules/kaheka
RHEINGAU=gauche-rheingau

build: $(TARGET)

run: $(TARGET) $(KAHEKA)
	LD_LIBRARY_PATH=$(LD_LIBRARY_PATH) ./$(TARGET)

## docker run --rm -p 2222:2222 -v$PWD:/code -w /code -t -i gauche-violet_gosh make debug
debug: $(TARGET) $(KAHEKA)
	gdb -ex run $(TARGET)

$(TARGET): main.c
	$(CC) -g -I/usr/local/include -o $(TARGET) main.c $(CFLAGS) $(LFLAGS)

$(KAHEKA): $(RHEINGAU)
	gosh -I. $(RHEINGAU)/rheingau.scm install kaheka

$(RHEINGAU):
	git clone https://github.com/torus/gauche-rheingau.git $(RHEINGAU)

clean:
	rm -rf *.o $(TARGET) gosh-modules $(RHEINGAU) $(TARGET).dSYM
