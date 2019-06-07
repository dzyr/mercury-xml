#-----------------------------------------------------------------------------%
# Any copyright is dedicated to the Public Domain.
# http://creativecommons.org/publicdomain/zero/1.0/
#-----------------------------------------------------------------------------%

PROJECT_DIR := $(shell dirname $(realpath $(lastword $(MAKEFILE_LIST))))


.PHONY: default
default:
	cd src && $(MAKE) INSTALL_PREFIX=$(PROJECT_DIR)/.sandbox default


 .PHONY: install
install:
	cd src && $(MAKE) INSTALL_PREFIX=$(PROJECT_DIR)/.sandbox install


.PHONY: test
test:
	cd tests && $(MAKE) test


.PHONY: realclean
realclean:
	cd src && $(MAKE) realclean
	cd tests && $(MAKE) realclean
	cd .sandbox && $(MAKE) realclean
