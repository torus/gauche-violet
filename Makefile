# violet - Web server based on Gauche and Libuv
# See LICENSE file for copyright and license details.

# mostly copied from: https://git.suckless.org/surf/log.html

include config.mk

SRC = main.c
OBJ = $(SRC:.c=.o)

RHEINGAU=./gauche-rheingau

all: options violet $(RHEINGAU)

options:
	@echo violet build options:
	@echo "CC            = $(CC)"
	@echo "CFLAGS        = $(VIOLETCFLAGS) $(CFLAGS)"
	@echo "LDFLAGS       = $(LDFLAGS) $(LIBS)"

violet: $(OBJ)
	$(CC) $(LDFLAGS) -o $@ $(OBJ) $(LIBS)

$(RHEINGAU):
	git clone https://github.com/torus/gauche-rheingau.git $(RHEINGAU)

$(OBJ) $(WOBJ): config.mk

config.h:
	cp config.def.h $@

$(OBJ): $(SRC)
	$(CC) $(VIOLETCFLAGS) $(CFLAGS) -c $(SRC)

clean:
	rm -f violet $(OBJ)
	rm -rf */*~ *~ *.o gosh-modules $(RHEINGAU) $(TARGET).dSYM

distclean: clean
	rm -f config.h violet-$(VERSION).tar.gz

dist: distclean
	mkdir -p violet-$(VERSION)
	cp -R Dockerfile Makefile README.md barrel.scm config.mk	\
	    docker-compose.yml docker.mk eg lib static violet.1		\
	    $(SRC) violet-$(VERSION)
	tar -cf violet-$(VERSION).tar violet-$(VERSION)
	gzip violet-$(VERSION).tar
	rm -rf violet-$(VERSION)

install: all
	mkdir -p $(DESTDIR)$(PREFIX)/bin
	cp -f violet $(DESTDIR)$(PREFIX)/bin
	chmod 755 $(DESTDIR)$(PREFIX)/bin/violet
	mkdir -p $(DESTDIR)$(LIBDIR)
	cp -f lib/violet.scm $(DESTDIR)$(LIBDIR)
	cd gauche-rheingau && ./configure
	$(MAKE) -C gauche-rheingau install
	mkdir -p $(DESTDIR)$(MANPREFIX)/man1
	sed "s/VERSION/$(VERSION)/g" < violet.1 > $(DESTDIR)$(MANPREFIX)/man1/violet.1
	chmod 644 $(DESTDIR)$(MANPREFIX)/man1/violet.1

uninstall:
	rm -f $(DESTDIR)$(PREFIX)/bin/violet
	rm -f $(DESTDIR)$(MANPREFIX)/man1/violet.1
	rm -f $(DESTDIR)$(LIBDIR)/violet.scm
	rmdir -f $(DESTDIR)$(LIBDIR)

.PHONY: all options distclean clean dist install uninstall
