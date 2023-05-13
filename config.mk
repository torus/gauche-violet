# Violet version
VERSION = 0.3.2-fix1

# paths
PREFIX = /usr/local
MANPREFIX = $(PREFIX)/share/man
LIBPREFIX = $(PREFIX)/lib
LIBDIR = $(LIBPREFIX)/violet

GAUCHEINC = $(shell gauche-config -I)
GAUCHELIB = $(shell gauche-config -l)
UVINC = $(shell pkg-config --cflags libuv)
UVLIB = $(shell pkg-config --libs libuv)

# includes and libs
INCS = $(GAUCHEINC) $(UVINC)
LIBS = $(GAUCHELIB) $(UVLIB)

# flags
CFLAGS = -DVERSION=\"$(VERSION)\" -DLIBPREFIX=\"$(LIBPREFIX)\" -DLIBDIR=\"$(LIBDIR)\" -Wall -Werror
VIOLETCFLAGS = $(INCS)
MAKIKI = $(LIBDIR)/makiki.scm
