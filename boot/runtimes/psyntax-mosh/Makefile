PSYNTAX_SOURCES = psyntax-buildscript-mosh.ss \
psyntax/builders.ss \
psyntax/compat.ss \
psyntax/config.ss \
psyntax/expander.ss \
psyntax/internal.ss \
psyntax/library-manager.ss \
psyntax/main.ss

ROOT_DIR= ../../..
MOSH=$(ROOT_DIR)/mosh

# 1. Expand psyntax.scm using Mosh.
# 2. Copy the generated psyntax.scm to /saved so that you can commit generated psyntax.scm just in case.
psyntax.scm: $(PSYNTAX_SOURCES)
	MOSH_GENSYM_PREFIX=`$(MOSH) print-prefix.ss` $(MOSH) psyntax-buildscript-mosh.ss
	cp psyntax.scm saved/
