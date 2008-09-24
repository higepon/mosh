all: titlecase-datum-inc.scm upcase-datum-inc.scm downcase-datum-inc.scm general-category-datum-inc.scm other-alphabetic-datum-inc.scm

data/UnicodeData.txt :
	(cd data && wget "http://www.unicode.org/Public/UNIDATA/UnicodeData.txt")

data/PropList.txt:
	(cd data && wget "http://www.unicode.org/Public/UNIDATA/PropList.txt")

titlecase-datum-inc.scm: make-title-case-datum.scm data/UnicodeData.txt
	mosh -b $< > $@

upcase-datum-inc.scm: make-upcase-datum.scm data/UnicodeData.txt
	mosh -b $< > $@

downcase-datum-inc.scm: make-downcase-datum.scm data/UnicodeData.txt
	mosh -b $< > $@

general-category-datum-inc.scm: make-general-category-datum.scm data/UnicodeData.txt
	mosh -b $< > $@

other-alphabetic-datum-inc.scm: make-other-alphabetic-datum.scm data/PropList.txt
	mosh -b $< > $@

clean:
	rm -rf *-inc.scm


