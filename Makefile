SCHEME := $(shell which scheme || which chez || which chezscheme)

all:
	@echo small libs in scheme, r6rs compatible.

.PHONY: test test_guile test_chez

test: test_guile test_chez

test_guile:
	guile --r6rs -L . bin/test.scm

.PHONY: test.txt

test.txt:
	echo guild compile -L . test/hello.scm

# $$(echo `basename $@` | sed 's/-/ /g' | sed 's/.test.txt//g')
build/%.chez.txt: test.txt
	$(SCHEME) --compile-imported-libraries --libdirs .::build/chezscheme --program bin/test.scm  test hello  | tee $@

build/%.guile.txt: test.txt
	guile --r6rs -L .  bin/test.scm  test hello  | tee $@

test_chez:
	$(SCHEME) --compile-imported-libraries --libdirs .::build/chezscheme --program bin/test.scm
