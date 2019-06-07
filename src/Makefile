#-----------------------------------------------------------------------------%
# Any copyright is dedicated to the Public Domain.
# http://creativecommons.org/publicdomain/zero/1.0/
#-----------------------------------------------------------------------------%

MAIN_MODULE := xml_read

.PHONY: default
default:
	mmc --make lib$(MAIN_MODULE)

.PHONY: install
install:
	mmc --make lib$(MAIN_MODULE).install

.PHONY: realclean
realclean:
	mmc --make $(MAIN_MODULE).realclean
	rm -rf Mercury
	rm -f $(wildcard *.err) $(wildcard *.mh)
