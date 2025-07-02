.PHONY: default all clean token.go parse.go conv.go hash.go

SRC = src

PREFIX ?= /usr
DATADIR ?= $(PREFIX)/share
LIBDIR ?= $(PREFIX)/lib

GUILE_SITE ?= $(DATADIR)/guile/site/3.0
GUILE_CCACHE ?= $(LIBDIR)/guile/3.0/site-ccache

GUILD = @guild compile -L $(SRC) -O2

default: token.go parse.go conv.go hash.go
all: default

install: all
	install -m 644 $(SRC)/god/*.scm      -Dt $(DESTDIR)$(GUILE_SITE)/god
	install -m 644 $(SRC)/god/conv/*.scm -Dt $(DESTDIR)$(GUILE_SITE)/god/conv
	install -m 644 $(SRC)/god/*.go       -Dt $(DESTDIR)$(GUILE_CCACHE)/god
	install -m 644 $(SRC)/god/conv/*.go  -Dt $(DESTDIR)$(GUILE_CCACHE)/god/conv

uninstall:
	-rm -f $(DESTDIR)$(GUILE_SITE)/god/*.scm
	-rm -f $(DESTDIR)$(GUILE_SITE)/god/conv/*.scm
	-rmdir $(DESTDIR)$(GUILE_SITE)/god
	-rm -f $(DESTDIR)$(GUILE_CCACHE)/god/*.go
	-rm -f $(DESTDIR)$(GUILE_CCACHE)/god/conv/*.go
	-rmdir $(DESTDIR)$(GUILE_CCACHE)/god

clean:
	-rm -f src/god/*.go src/god/*/*.go

token.go:
	$(GUILD) $(SRC)/god/token.scm -o $(SRC)/god/token.go

parse.go:
	$(GUILD) $(SRC)/god/parse.scm -o $(SRC)/god/parse.go

conv.go:
	$(GUILD) $(SRC)/god/conv.scm -o $(SRC)/god/conv.go

hash.go:
	$(GUILD) $(SRC)/god/conv/hash.scm -o $(SRC)/god/conv/hash.go