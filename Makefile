TARGET=violet
CFLAGS=$(shell gauche-config -I)
LFLAGS=-luv $(shell gauche-config -L) -lgauche-0.9

KAHEKA=gosh-modules/kaheka
RHEINGAU=gauche-rheingau

build: $(TARGET)

run: $(TARGET) $(KAHEKA)
	./$(TARGET)

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
