all: titlecase-datum-inc.scm \
upcase-datum-inc.scm \
downcase-datum-inc.scm \
general-category-datum-inc.scm \
other-alphabetic-property-datum-inc.scm \
other-uppercase-property-datum-inc.scm \
other-lowercase-property-datum-inc.scm \
whitespace-property-datum-inc.scm \
special-uppercase-datum-inc.scm

data/UnicodeData.txt :
	(cd data && wget "http://www.unicode.org/Public/UNIDATA/UnicodeData.txt")

data/PropList.txt:
	(cd data && wget "http://www.unicode.org/Public/UNIDATA/PropList.txt")

data/SpecialCasing.txt:
	(cd data && wget "http://www.unicode.org/Public/UNIDATA/SpecialCasing.txt")

special-uppercase-datum-inc.scm: make-special-uppercase-datum.scm data/SpecialCasing.txt
	mosh -b $< > $@

titlecase-datum-inc.scm: make-title-case-datum.scm data/UnicodeData.txt
	mosh -b $< > $@

upcase-datum-inc.scm: make-upcase-datum.scm data/UnicodeData.txt
	mosh -b $< > $@

downcase-datum-inc.scm: make-downcase-datum.scm data/UnicodeData.txt
	mosh -b $< > $@

general-category-datum-inc.scm: make-general-category-datum.scm data/UnicodeData.txt
	mosh -b $< > $@

other-alphabetic-property-datum-inc.scm: make-other-alphabetic-property-datum.scm data/PropList.txt
	mosh -b $< > $@

other-uppercase-property-datum-inc.scm: make-other-uppercase-property-datum.scm data/PropList.txt
	mosh -b $< > $@

other-lowercase-property-datum-inc.scm: make-other-lowercase-property-datum.scm data/PropList.txt
	mosh -b $< > $@


whitespace-property-datum-inc.scm: make-whitespace-property-datum.scm data/PropList.txt
	mosh -b $< > $@


clean:
	rm -rf *-inc.scm


