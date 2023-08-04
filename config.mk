# Violet version
VERSION = 0.3.2-fix1

# paths
PREFIX = $(PWD)/hack/local
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
