all: nmosh_image.cpp

.PHONY: p0 p0gauche p1 p1gauche boot0

p1: p1gauche
p0: p0gauche

bootstrap.exp: p1
bootstrap0.exp: p0

bootstrap:
	mosh -5 bootstrap.vanilla-mosh/phase01.vanilla-mosh.scm

boot0: p0
	mosh -5 bootstrap.vanilla-mosh/phase1.vanilla-mosh.scm

p1gauche:
	gosh bootstrap.gauche/phase1.gauche.scm
	sed -e "s/#\\\\null/#\\\\nul/g;s/|\.\.\.|/\.\.\./g;s/|@|/@/g" bootstrap.gexp > bootstrap.exp

p0gauche:
	gosh bootstrap.gauche/phase0.gauche.scm
	sed -e "s/#\\\\null/#\\\\nul/g;s/|\.\.\.|/\.\.\./g;s/|@|/@/g" bootstrap0.gexp > bootstrap0.exp

nmosh_image.cpp: p1
	mosh -5 bootstrap.vanilla-mosh/phase2.scm

test: all 
	../../../mosh -5 boot5.scm tests/r6rs/run.sps

inject-nmosh:
	nmosh build-run.ss
	cp nmosh_image.cpp ../../../src/
clean:
	rm -f *.exp
	rm -f *.exp-e
	rm -f *.gexp
	rm -f *.nmosh-dbg
	rm -f nmosh_image.cpp
