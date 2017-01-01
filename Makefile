TARGET=violet
GAUCHEDIR=../Gauche
GAUCHELIBDIR=../Gauche/src
CFLAGS=-I$(GAUCHEDIR)/src -I$(GAUCHEDIR)/gc/include
LFLAGS=-L/usr/local/lib -luv -L$(GAUCHELIBDIR) -lgauche-0.9 -lcurl-gnutls

build: $(TARGET)

run: $(TARGET)
	LD_LIBRARY_PATH=$(GAUCHELIBDIR) ./$(TARGET)

$(TARGET): main.c
	$(CC) -g -I/usr/local/include -o $(TARGET) main.c $(CFLAGS) $(LFLAGS)
