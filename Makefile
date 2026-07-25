THIS_MAKEFILE_DIR = $(abspath $(dir $(lastword $(MAKEFILE_LIST))))
EMACS ?= emacs
SRC=oauth2-auto.el
TEST=test/oauth2-auto-test.el
BUILD_LOG = build.log
CASK ?= cask
PKG_DIR := $(shell $(CASK) package-directory)
ELCFILES = $(SRC:.el=.elc) $(TEST:.el=.elc)
.DEFAULT_GOAL := all

.PHONY: all clean load-path compile test test-checkdoc test-unit elpa

all: compile test

clean:
	rm -f $(ELCFILES) $(BUILD_LOG)
	rm -rf $(PKG_DIR)
	$(CASK) clean-elc

elpa: $(PKG_DIR)
$(PKG_DIR): Cask
	$(CASK) install
	touch $@

compile: $(SRC) $(TEST) elpa
	$(CASK) build 2>&1 | tee $(BUILD_LOG); \
	! ( grep -E -e ':(Warning|Error):' $(BUILD_LOG) )
	$(CASK) emacs --batch -L . -L test \
		-f batch-byte-compile $(TEST)

test: test-checkdoc test-unit

test-checkdoc: $(SRC) elpa compile
	FILES="$(SRC)" $(CASK) emacs --batch \
		-L $(THIS_MAKEFILE_DIR) \
		-l $(THIS_MAKEFILE_DIR)/$(TEST) \
		-f oauth2-auto-test-checkdoc

test-unit: $(SRC) $(TEST) elpa compile
	$(CASK) emacs --batch \
		-L $(THIS_MAKEFILE_DIR) \
		$(foreach test,$(TEST),$(addprefix -l $(THIS_MAKEFILE_DIR)/,$(test))) \
		-f ert-run-tests-batch-and-exit
