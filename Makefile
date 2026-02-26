EMACS ?= emacs

TEST_DIR=$(shell pwd)/test

# Run all tests by default.
MATCH ?=

.PHONY: test

test:
	$(EMACS) --batch -L . -L $(TEST_DIR) -l org-people-test.el -eval '(ert-run-tests-batch-and-exit "$(MATCH)")'
