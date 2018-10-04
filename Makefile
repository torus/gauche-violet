TARGET=violet
CFLAGS=$(shell gauche-config -I)
LFLAGS=-luv $(shell gauche-config -L) -lgauche-0.9

KAHEKA=gosh-modules/kaheka
RHEINGAU=gauche-rheingau

build: $(TARGET)

run: $(TARGET) $(KAHEKA)
	./$(TARGET)

$(TARGET): main.c
	$(CC) -g -I/usr/local/include -o $(TARGET) main.c $(CFLAGS) $(LFLAGS)

$(KAHEKA): $(RHEINGAU)
	gosh $(RHEINGAU)/rheingau.scm install kaheka

$(RHEINGAU):
	git clone git@github.com:torus/gauche-rheingau.git $(RHEINGAU)

clean:
	rm -rf *.o $(TARGET) $(KAHEKA) $(RHEINGAU)
