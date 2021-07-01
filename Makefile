# violet - Web server based on Gauche and Libuv
# See LICENSE file for copyright and license details.

# mostly copied from: https://git.suckless.org/surf/log.html

include config.mk

SRC = main.c
OBJ = $(SRC:.c=.o)

all: options violet

options:
	@echo violet build options:
	@echo "CC            = $(CC)"
	@echo "CFLAGS        = $(VIOLETCFLAGS) $(CFLAGS)"
	@echo "LDFLAGS       = $(LDFLAGS) $(LIBS)"
	@echo "MAKIKI        = $(MAKIKI)"

violet: $(OBJ)
	$(CC) $(LDFLAGS) -o $@ $(OBJ) $(LIBS)

$(OBJ) $(WOBJ): config.mk

config.h:
	cp config.def.h $@

$(OBJ): $(SRC)
	$(CC) $(VIOLETCFLAGS) $(CFLAGS) -c $(SRC)

clean:
	rm -f violet $(OBJ)
	rm -rf */*~ *~ *.o gosh-modules $(TARGET).dSYM

distclean: clean
	rm -f config.h violet-$(VERSION).tar.gz

dist: distclean
	mkdir -p violet-$(VERSION)
	cp -R Dockerfile Makefile README.md config.mk eg lib violet.1 \
	    $(SRC) violet-$(VERSION)
	tar -cf violet-$(VERSION).tar violet-$(VERSION)
	gzip violet-$(VERSION).tar
	rm -rf violet-$(VERSION)

install: all $(MAKIKI)
	mkdir -p $(PREFIX)/bin
	cp -f violet $(PREFIX)/bin
	chmod 755 $(PREFIX)/bin/violet
	mkdir -p $(LIBDIR)
	cp -f lib/violet.scm $(LIBDIR)
	mkdir -p $(MANPREFIX)/man1
	sed "s/VERSION/$(VERSION)/g" < violet.1 > $(MANPREFIX)/man1/violet.1
	chmod 644 $(MANPREFIX)/man1/violet.1

$(MAKIKI):
	mkdir -p $(LIBDIR)
	curl -o $@ https://raw.githubusercontent.com/shirok/Gauche-makiki/use-connection/makiki.scm

uninstall:
	rm -f $(PREFIX)/bin/violet
	rm -f $(MANPREFIX)/man1/violet.1
	rm -f $(LIBDIR)/violet.scm
	rmdir -f $(LIBDIR)

push-docker:
	sudo docker pull practicalscheme/gauche:latest
	sudo docker build -t torus/violet:latest .
	sudo docker push torus/violet:latest

.PHONY: all options distclean clean dist install uninstall
