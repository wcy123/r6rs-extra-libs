SCHEME := $(shell which scheme || which chez || which chezscheme)

all:
	@echo small libs in scheme, r6rs compatible.

.PHONY: test test_guile test_chez

test: test_guile test_chez

test_guile:
	guile --r6rs -L . test/test.scm

compile:
	guild compile -L . -o /tmp/a.o test/repl.scm
test_chez:
	$(SCHEME) --compile-imported-libraries --libdirs .::build/chezscheme --program test/test.scm
