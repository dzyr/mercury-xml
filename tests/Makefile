#-----------------------------------------------------------------------------%
# Any copyright is dedicated to the Public Domain.
# http://creativecommons.org/publicdomain/zero/1.0/
#-----------------------------------------------------------------------------%

MAIN_MODULE := test_xml


.PHONY: test
test: $(MAIN_MODULE)
	@./$(MAIN_MODULE)

.PHONY: test-verbose
test-verbose: $(MAIN_MODULE)
	@./$(MAIN_MODULE) -v

$(MAIN_MODULE): Mercury.modules
	mmc --make $@

Mercury.modules: $(wildcard *.m) $(wildcard ../src/*.m)
	mmc -f $(wildcard *.m) $(wildcard ../src/*.m)

.PHONY: benchmark
benchmark: $(MAIN_MODULE) large.xml
	@./$(MAIN_MODULE) -b

large.xml:
	@./$(MAIN_MODULE) -w

.PHONY: realclean
realclean:
	mmc --make $(MAIN_MODULE).realclean
	rm -rf Mercury
	rm -f $(wildcard *.err) $(wildcard *.mh)
	rm -f Mercury.modules
	rm -f FAILED_TESTS ABORTED_TESTS
	rm -f large.xml
