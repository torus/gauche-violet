# Violet version
VERSION = 0.4.0

# paths
PREFIX = /usr/local
MANPREFIX = $(PREFIX)/share/man
LIBPREFIX = $(PREFIX)/lib
LIBDIR = $(LIBPREFIX)/violet

GAUCHEINC = $(shell $(PREFIX)/bin/gauche-config -I)
GAUCHELIB = $(shell $(PREFIX)/bin/gauche-config -l)
GAUCHELIBDIR = $(shell $(PREFIX)/bin/gauche-config -L)
UVINC = $(shell pkg-config --cflags libuv)
UVLIB = $(shell pkg-config --libs libuv)

# includes and libs
INCS = $(GAUCHEINC) $(UVINC)
LIBS = $(GAUCHELIBDIR) $(GAUCHELIB) $(UVLIB)

# flags
CFLAGS = -DVERSION=\"$(VERSION)\" -DLIBPREFIX=\"$(LIBPREFIX)\" -DLIBDIR=\"$(LIBDIR)\" -Wall -Werror
VIOLETCFLAGS = $(INCS)
MAKIKI = $(LIBDIR)/makiki.scm
