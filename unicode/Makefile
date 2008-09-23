
all: downcase-inc.scm

downcase-inc.scm: data/CaseFolding.txt make-case-data.scm
	cat ../data/header.scm > $@
	mosh -b make-case-data.scm >> $@

clean:
	rm -rf downcase-inc.scm
