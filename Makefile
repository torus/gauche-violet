TARGET=tcp-echo-server
GAUCHEDIR=../Gauche
CFLAGS=-I$(GAUCHEDIR)/src -I$(GAUCHEDIR)/gc/include
LIBGAUCHE=Gauche/libgauche-0.9
# LIBGAUCHE=Gauche/libgauche-static-0.9.a
LFLAGS=-L/usr/local/lib -luv -LGauche -lgauche-0.9
# LFLAGS=-L/usr/local/lib -luv -LGauche -lgauche-static-0.9

build: $(TARGET)

run: $(TARGET)
	./$(TARGET)

$(TARGET): main.c $(LIBGAUCHE)
	$(CC) -g -I/usr/local/include -o $(TARGET) main.c $(CFLAGS) $(LFLAGS)

$(LIBGAUCHE):
	@echo Please build libgauche.a by make static
