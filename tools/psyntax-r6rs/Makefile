all:

# all: psyntax-ikarus.pp     psyntax-petite.pp   psyntax-mzscheme.pp \
#      psyntax-gambit.pp     psyntax-gosh.pp     psyntax-chicken.pp \
#      psyntax-larceny.pp    psyntax-sisc.pp     psyntax-bigloo.pp \
#      psyntax-mit-scheme.pp psyntax-scheme48.pp

# SCHEME_SRC=psyntax-buildscript.ss psyntax/builders.ss psyntax/compat.ss\
#            psyntax/config.ss psyntax/expander.ss psyntax/internal.ss\
#            psyntax/library-manager.ss psyntax/main.ss

# psyntax-ikarus.pp psyntax.pp: $(SCHEME_SRC)
# 	ikarus --r6rs-script psyntax-buildscript.ss
# 	cp psyntax.pp psyntax-ikarus.pp

# psyntax-petite.pp: psyntax-ikarus.pp petite.r6rs.ss
# 	cp psyntax-ikarus.pp psyntax.pp
# 	petite --script petite.r6rs.ss psyntax-buildscript.ss
# 	petite --script petite.r6rs.ss psyntax-buildscript.ss
# 	cp psyntax.pp psyntax-petite.pp

# psyntax-gambit.pp: psyntax-petite.pp gsi.r6rs.ss
# 	cp psyntax-petite.pp psyntax.pp
# 	./gsi.r6rs.ss psyntax-buildscript.ss 
# 	./gsi.r6rs.ss psyntax-buildscript.ss 
# 	cp psyntax.pp psyntax-gambit.pp

# psyntax-chicken.pp: psyntax-petite.pp csi.r6rs.ss
# 	cp psyntax-petite.pp psyntax.pp
# 	./csi.r6rs.ss psyntax-buildscript.ss 
# 	./csi.r6rs.ss psyntax-buildscript.ss 
# 	cp psyntax.pp psyntax-chicken.pp

# psyntax-gosh.pp: psyntax-petite.pp gosh.r6rs.ss
# 	cp psyntax-petite.pp psyntax.pp
# 	gosh gosh.r6rs.ss psyntax-buildscript.ss 
# 	gosh gosh.r6rs.ss psyntax-buildscript.ss 
# 	cp psyntax.pp psyntax-gosh.pp

# psyntax-larceny.pp: psyntax-petite.pp larceny.r6rs.ss
# 	cp psyntax-petite.pp psyntax.pp
# 	larceny -- larceny.r6rs.ss psyntax-buildscript.ss 
# 	larceny -- larceny.r6rs.ss psyntax-buildscript.ss 
# 	cp psyntax.pp psyntax-larceny.pp

# psyntax-mzscheme.pp: psyntax-petite.pp mzscheme.r6rs.ss
# 	cp psyntax-petite.pp psyntax.pp
# 	mzscheme -f mzscheme.r6rs.ss -- psyntax-buildscript.ss 
# 	mzscheme -f mzscheme.r6rs.ss -- psyntax-buildscript.ss 
# 	cp psyntax.pp psyntax-mzscheme.pp

# psyntax-sisc.pp: psyntax-petite.pp sisc.r6rs.ss
# 	cp psyntax-petite.pp psyntax.pp
# 	sisc -x sisc.r6rs.ss -c start-r6rs -- psyntax-buildscript.ss 
# 	sisc -x sisc.r6rs.ss -c start-r6rs -- psyntax-buildscript.ss 
# 	cp psyntax.pp psyntax-sisc.pp

# psyntax-bigloo.pp: psyntax-petite.pp bigloo.r6rs.ss
# 	cp psyntax-petite.pp psyntax.pp
# 	echo '(load "bigloo.r6rs.ss")' | bigloo -i -- psyntax-buildscript.ss 
# 	echo '(load "bigloo.r6rs.ss")' | bigloo -i -- psyntax-buildscript.ss 
# 	cp psyntax.pp psyntax-bigloo.pp

# psyntax-mit-scheme.pp: psyntax-petite.pp mit-scheme.r6rs.ss
# 	cp psyntax-petite.pp psyntax.pp
# 	echo "" | mit-scheme -heap 10240 -load mit-scheme.r6rs.ss 
# 	echo "" | mit-scheme -heap 10240 -load mit-scheme.r6rs.ss 
# 	cp psyntax.pp psyntax-mit-scheme.pp

# psyntax-scheme48.pp: psyntax-petite.pp scheme48.r6rs.ss
# 	(cp psyntax-petite.pp psyntax.pp) && \
# 	(cat scheme48.r6rs.ss | scheme48 -h 100000000) && \
# 	(cat scheme48.r6rs.ss | scheme48 -h 100000000) && \
# 	(cp psyntax.pp psyntax-scheme48.pp)


# clean:
# 	rm -f *.pp
