all :
	# nothing

# NB. also modify \newcommand{\version} in doc/cefp.tex
VERSION = cadarache
BRANCH = cadarache

TARBALL = par-tutorial-$(VERSION).tar.gz
ZIPBALL = par-tutorial-$(VERSION).zip

$(TARBALL):
	git archive --format tar --prefix=par-tutorial/ $(BRANCH) . | gzip >$@

$(ZIPBALL):
	git archive --format zip --prefix=par-tutorial/ $(BRANCH) . -o $@

.PHONY: tar
tar : $(TARBALL)
.PHONY: zip
zip : $(ZIPBALL)

upload : $(TARBALL) $(ZIPBALL)
	scp $(TARBALL) community:public_html
	scp $(ZIPBALL) community:public_html
	(cd doc; make upload)
