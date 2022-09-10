PSYNTAX_SOURCES = psyntax-buildscript-mosh.ss \
psyntax/builders.ss \
psyntax/compat.ss \
psyntax/config.ss \
psyntax/expander.ss \
psyntax/internal.ss \
psyntax/library-manager.ss \
psyntax/main.ss

ROOT_DIR= ../../..
MOSH=mosh

# Expand psyntax.scm using pre-installed stable Mosh.
psyntax.scm: $(PSYNTAX_SOURCES)
	$(MOSH) psyntax-buildscript-mosh.ss
