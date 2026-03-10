EMACS ?= emacs

# Run all tests by default.
MATCH ?=

.PHONY: test

test:
	cd test/ && $(EMACS) --batch -L . -L .. -l org-people-test.el -eval '(ert-run-tests-batch-and-exit "$(MATCH)")'

clean:
	find . -name '*.elc' -delete

org-people.elc: org-people.el
	$(EMACS) --batch -L . -l org-people.el -eval '(byte-compile-file "org-people.el")'

bytecompile: org-people.elc
