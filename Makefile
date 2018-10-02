TARGET=violet
GAUCHELIBDIR=/usr/local/lib/gauche-0.9/0.9.6/x86_64-apple-darwin17.7.0/
CFLAGS=-I/usr/local/lib/gauche-0.9/0.9.6/include
LFLAGS=-L/usr/local/lib -luv -L$(GAUCHELIBDIR) -lgauche-0.9 -lcurl

build: $(TARGET)

run: $(TARGET)
	LD_LIBRARY_PATH=$(GAUCHELIBDIR) ./$(TARGET)

$(TARGET): main.c
	$(CC) -g -I/usr/local/include -o $(TARGET) main.c $(CFLAGS) $(LFLAGS)
