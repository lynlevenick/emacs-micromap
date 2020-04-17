.POSIX:

.PHONY: all clean compile test
.SUFFIXES: .el .elc

EMACS = emacs
LDFLAGS = -L . -L ../m

PACKAGE = micromap
VERSION = 1.0.0

# Files

DOC =
EL = micromap.el

# Targets

all: compile

clean:
	rm -f $(EL:.el=.elc) $(PACKAGE)-$(VERSION).tar

compile: $(EL:.el=.elc)

package: $(PACKAGE)-$(VERSION).tar

# Outputs

$(PACKAGE)-$(VERSION).tar: $(EL) $(DOC)
	tar -cf $@ $^

# Suffix rules

.el.elc:
	$(EMACS) -Q -batch -L . $(LDFLAGS) -f batch-byte-compile $<
