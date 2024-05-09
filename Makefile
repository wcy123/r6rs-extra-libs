CHEZ := $(shell which scheme || which chez || which chezscheme)

RUN_CHEZ := $(CHEZ) --compile-imported-libraries --libdirs .::build/chezscheme --program
RUN_GUILE := guile --r6rs -L .
SHELL := /bin/bash

all:
	@echo small libs in scheme, r6rs compatible.

.PHONY: test

test: test.txt
	@echo; echo "MAKE: TEST FINISH"


.PHONY: test.txt
test.txt: $(patsubst test/%.scm, build/%.txt, $(wildcard test/*.scm test/*/*/*.scm test/*/*/*/*.scm))
	@echo; echo "MAKE: ALL TEST OK"

.PHONY: always
build/%.txt: build/%.chez-txt build/%.guile-txt always
	@echo; echo "MAKE: ("$(wordlist  2,10, $(subst /, ,$(<D)) $(basename $(<F)))") IS OK with all scheme implementations"

build/%.chez-txt: test/%.scm always
	@set -o pipefail; echo; echo "MAKE: testing ("$(subst /, ,$(<D)) $(basename $(<F))") with "$(CHEZ) && \
	mkdir -p $(@D) && $(RUN_CHEZ)  bin/test.scm $(subst /, ,$(<D)) $(basename $(<F)) 2>&1 | tee $@ && \
	echo "MAKE: testing ("$(subst /, ,$(<D)) $(basename $(<F))") with $(CHEZ) OK"

build/%.guile-txt: test/%.scm always
	@set -o pipefail; echo; echo "MAKE: testing ("$(subst /, ,$(<D)) $(basename $(<F))") with guile" && \
	mkdir -p $(@D) && $(RUN_GUILE) bin/test.scm $(subst /, ,$(<D)) $(basename $(<F)) 2>&1 | tee $@ && \
	echo "MAKE: testing ("$(subst /, ,$(<D)) $(basename $(<F))") with guile OK"
