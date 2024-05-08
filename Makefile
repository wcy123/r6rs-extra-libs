CHEZ := $(shell which scheme || which chez || which chezscheme)

RUN_CHEZ := $(CHEZ) --compile-imported-libraries --libdirs .::build/chezscheme --program
RUN_GUILE := guile --r6rs -L .

all:
	@echo small libs in scheme, r6rs compatible.

.PHONY: test

test: test.txt
	@echo "TEST DONE"


.PHONY: test.txt
test.txt: build/hello.txt build/hello/world.txt
	@echo "ALL OK"

.PHONY: always

xx:
	echo $(patsubst %.scm, %.txt, $(wildcard test/*.scm test/**/*.scm))

build/%.txt: build/%.chez-txt build/%.guile-txt always
	@echo "("$(wordlist  2,10, $(subst /, ,$(<D)) $(basename $(<F)))")" OK

build/%.chez-txt: test/%.scm
	@echo "testing ("$(subst /, ,$(<D)) $(basename $(<F))") with "$(CHEZ) && \
	mkdir -p $(@D) && $(RUN_CHEZ)  bin/test.scm $(subst /, ,$(<D)) $(basename $(<F)) | tee $@

build/%.guile-txt: test/%.scm always
	@echo "testing ("$(subst /, ,$(<D)) $(basename $(<F))") with guile" && \
	mkdir -p $(@D) && $(RUN_GUILE) bin/test.scm $(subst /, ,$(<D)) $(basename $(<F)) | tee $@
