THIS_MAKEFILE_DIR = $(abspath $(dir $(lastword $(MAKEFILE_LIST))))
EMACS ?= emacs
SRC=oauth2-auto.el
TEST=
BUILD_LOG = build.log
CASK ?= cask
PKG_DIR := $(shell $(CASK) package-directory)
ELCFILES = $(SRC:.el=.elc)
.DEFAULT_GOAL := all

.PHONY: all clean load-path compile test test-checkdoc test-unit elpa

# Remove test target until tests are implemented.
all: compile test

clean:
	rm -f $(ELCFILES) $(BUILD_LOG); rm -rf $(PKG_DIR)

elpa: $(PKG_DIR)
$(PKG_DIR): Cask
	$(CASK) install
	touch $@

compile: $(SRC) elpa
	$(CASK) build 2>&1 | tee $(BUILD_LOG); \
	! ( grep -E -e ':(Warning|Error):' $(BUILD_LOG) )

# Remove test-unit target until unit tests are implemented.
test: test-checkdoc

test-checkdoc: $(SRC) elpa compile
	FILES="$(SRC)" $(CASK) eval '\
	(let* ((files (split-string (getenv "FILES") " "))) \
	  (dolist (file files) \
	    (let ((pkg (intern-soft (file-name-base file)))) \
	      (require pkg) \
	      (checkdoc-file file))) \
	  (when (get-buffer "*Warnings*") \
	    (kill-emacs 1)))'

test-unit: $(SRC) $(TEST) elpa
	echo Tests not yet implemented; exit 1; \
	$(CASK) exec ert-runner -L $(THIS_MAKEFILE_DIR) \
		$(foreach test,$(TEST),$(addprefix $(THIS_MAKEFILE_DIR)/,$(test)))
