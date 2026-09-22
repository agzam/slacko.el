.PHONY: help test deps check-compile compile clean

ELPA := $(CURDIR)/.elpa
SANDBOX := $(CURDIR)/.sandbox

# Every Emacs here runs against a throwaway init directory and a
# repo-local package directory: neither -Q nor --batch moves
# `user-emacs-directory', so an unset one writes into whatever
# ~/.emacs.d the developer actually uses.
EMACS := emacs -Q --batch --init-directory=$(SANDBOX) --eval '(setq package-user-dir "$(ELPA)")'

SOURCES := slacko-mrkdwn.el slacko-creds.el slacko-render.el \
	slacko-reactions.el slacko.el slacko-thread.el slacko-emoji.el \
	slacko-consult.el

# consult and embark are not dependencies of the package; they are here
# so the tests can exercise the integration where they are installed
define DEPS_SCRIPT
(progn
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)
(package-refresh-contents)
(dolist (pkg '(buttercup emojify consult embark))
  (unless (package-installed-p pkg)
    (package-install pkg))))
endef
export DEPS_SCRIPT

help:
	@echo "Available commands:"
	@echo "  make deps          Install dependencies into .elpa"
	@echo "  make test          Run the tests"
	@echo "  make compile       Byte-compile the package"
	@echo "  make check-compile Check for clean byte-compilation"
	@echo "  make clean         Remove compiled files and the sandbox"

deps:
	@echo "Installing dependencies into $(ELPA)"
	$(EMACS) --eval "$$DEPS_SCRIPT"

test:
	$(EMACS) -f package-initialize -L . -L test -f buttercup-run-discover

check-compile: deps
	@echo "Checking byte-compilation..."
	@for f in $(SOURCES); do \
	  $(EMACS) -f package-initialize -L . \
	    --eval "(setq byte-compile-error-on-warn t)" \
	    --eval "(byte-compile-file \"$$f\")" || exit 1; \
	done
	@echo "Checking slacko-consult.el compiles without Consult..."
	@$(EMACS) -L . \
	  --eval "(setq byte-compile-error-on-warn t)" \
	  --eval '(byte-compile-file "slacko-consult.el")'

compile:
	@echo "Byte-compiling package files..."
	@for f in $(SOURCES); do \
	  $(EMACS) -f package-initialize -L . \
	    --eval "(byte-compile-file \"$$f\")" || exit 1; \
	done

clean:
	@echo "Cleaning compiled files..."
	rm -f *.elc test/*.elc
	rm -rf $(ELPA) $(SANDBOX)
