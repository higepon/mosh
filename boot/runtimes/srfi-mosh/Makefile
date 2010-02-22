all: nmosh_image.cpp

.PHONY: p0 p0gauche p1 p1gauche boot0 p0vanilla-mosh

p1: p1psyntax-mosh
p0: p0vanilla-mosh

p1psyntax-mosh: p0
	mosh bootstrap.psyntax-mosh/phase1.psyntax-mosh.ss

p0vanilla-mosh:
	mosh -5 bootstrap.vanilla-mosh/phase0.vanilla-mosh.scm

p1gauche:
	gosh bootstrap.gauche/phase1.gauche.scm
	sed -e "s/#\\\\null/#\\\\nul/g;s/|\.\.\.|/\.\.\./g;s/|@|/@/g" bootstrap.gexp > bootstrap.exp

p0gauche:
	gosh bootstrap.gauche/phase0.gauche.scm
	sed -e "s/#\\\\null/#\\\\nul/g;s/|\.\.\.|/\.\.\./g;s/|@|/@/g" bootstrap0.gexp > bootstrap0.exp

# !CONFIG! : which bootstrap strategy ? 
#   p0 (use Phase0) 
#   p1 (use Phase1 - needs R5RS interpreter)
nmosh_image.cpp: p1 
	mosh -5 bootstrap.vanilla-mosh/phase2.scm

inject-nmosh: nmosh_image.cpp
	cp nmosh_image.cpp ../../../src/

# private target. DO NOT USE!
INJ:
	nmosh build-run.ss
	cp nmosh_image.cpp ../../../src/

clean:
	rm -f BOOT0.*
	rm -f BOOT1.*
	rm -f *.exp
	rm -f *.exp-e
	rm -f *.gexp
	rm -f *.nmosh-dbg
	rm -f nmosh_image.cpp
