NAME = org-people
EMACS ?= emacs

# Run all tests by default.
MATCH ?=

.PHONY: test

test:
	cd test/ && $(EMACS) --batch -L . -L .. -l ${NAME}-test.el -eval '(ert-run-tests-batch-and-exit "$(MATCH)")'

clean:
	find . -name '*.elc' -delete

${NAME}.elc: ${NAME}.el
	$(EMACS) --batch -L . -l ${NAME}.el -eval "(byte-compile-file \"${NAME}.el\")"

bytecompile: ${NAME}.elc
