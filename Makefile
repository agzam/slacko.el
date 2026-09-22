.PHONY: help deps test lint checkdoc melpazoid check-compile compile clean

ELPA := $(CURDIR)/.elpa
SANDBOX := $(CURDIR)/.sandbox
MELPAZOID ?= $(HOME)/GitHub/riscy/melpazoid/melpazoid/melpazoid.el

SOURCES := slacko-mrkdwn.el slacko-creds.el slacko-render.el \
	slacko-reactions.el slacko.el slacko-thread.el slacko-emoji.el \
	slacko-consult.el

SOURCES_EL := $(foreach f,$(SOURCES),\"$(f)\")

# Every Emacs here runs against a throwaway init directory and a
# repo-local package directory: neither -Q nor --batch moves
# `user-emacs-directory', so an unset one writes into whatever
# ~/.emacs.d the developer actually uses.  A build without native
# compilation still defines `startup-redirect-eln-cache' while leaving
# the variable it assigns to unbound, hence the second guard.
EMACS_Q = emacs -Q --init-directory "$(SANDBOX)" \
	--eval "(when (and (featurep 'native-compile) (fboundp 'startup-redirect-eln-cache)) (startup-redirect-eln-cache \"$(SANDBOX)/eln-cache/\"))" \
	--eval "(setq package-user-dir \"$(ELPA)\")" \
	--eval "(require 'package)" \
	--eval "(add-to-list 'package-archives '(\"melpa\" . \"https://melpa.org/packages/\"))"

EMACS = $(EMACS_Q) --batch

# consult, embark and emojify are not dependencies of the package; they
# are here so the tests can exercise the integration where they are
# installed.  package-lint and pkg-info are the lint harness, pkg-info
# being what melpazoid loads.
define DEPS_SCRIPT
(progn
(package-initialize)
(package-refresh-contents)
(dolist (pkg '(buttercup emojify consult embark package-lint pkg-info))
  (unless (package-installed-p pkg)
    (package-install pkg))))
endef
export DEPS_SCRIPT

help:
	@echo "Available commands:"
	@echo "  make deps          Install dependencies into .elpa"
	@echo "  make test          Run the tests"
	@echo "  make lint          Run package-lint"
	@echo "  make checkdoc      Run checkdoc"
	@echo "  make melpazoid     Run the harness a MELPA reviewer runs"
	@echo "  make compile       Byte-compile the package"
	@echo "  make check-compile Check for clean byte-compilation"
	@echo "  make clean         Remove compiled files and the sandbox"

deps:
	@echo "Installing dependencies into $(ELPA)"
	$(EMACS) --eval "$$DEPS_SCRIPT"

$(ELPA):
	@$(MAKE) deps

test:
	$(EMACS) -f package-initialize -L . -L test -f buttercup-run-discover

# never filtered: MELPA's CI does not filter either, so a warning hidden
# here surfaces during review instead of before it
lint: | $(ELPA)
	$(EMACS) -f package-initialize -L . \
	  --eval "(require 'package-lint)" \
	  --eval "(setq package-lint-main-file \"slacko.el\")" \
	  -f package-lint-batch-and-exit $(SOURCES)

checkdoc:
	@out=$$($(EMACS_Q) --batch \
	  --eval "(require 'checkdoc)" \
	  --eval "(dolist (f '($(SOURCES_EL))) (checkdoc-file f))" 2>&1); \
	if [ -n "$$out" ]; then echo "$$out"; exit 1; else echo "checkdoc: clean"; fi

# melpazoid's own Docker image has no arm64 build and its LOCAL_REPO path
# dies on the unix sockets a working tree holds, so load the elisp direct
melpazoid: | $(ELPA)
	@test -f "$(MELPAZOID)" || { echo "no melpazoid at $(MELPAZOID)"; exit 1; }
	@# it resolves dependencies through (locate-user-emacs-file "elpa")
	@ln -sfn "$(ELPA)" "$(SANDBOX)/elpa"
	@# it checks every .el beside the file it is pointed at, so stage only
	@# what ships - not the test suite, not the checkout
	@stage=$$(mktemp -d) && cp $(SOURCES) $$stage/ && \
	out=$$(cd $$stage && PACKAGE_MAIN=slacko.el $(EMACS) -f package-initialize \
	  --load="$(MELPAZOID)" 2>&1); \
	rm -rf $$stage; \
	if [ -n "$$out" ]; then echo "$$out"; exit 1; else echo "melpazoid: clean"; fi

# the .elc files go at the end: `locate-library' prefers them, so one
# left behind makes a later `make test' run against a stale artifact
check-compile: | $(ELPA)
	@echo "Checking byte-compilation..."
	@for f in $(SOURCES); do \
	  $(EMACS) -f package-initialize -L . \
	    --eval "(setq byte-compile-error-on-warn t)" \
	    --eval "(byte-compile-file \"$$f\")" || { rm -f *.elc; exit 1; }; \
	done
	@echo "Checking the optional integrations compile without their package..."
	@for f in slacko-consult.el slacko-emoji.el; do \
	  $(EMACS) -L . \
	    --eval "(setq byte-compile-error-on-warn t)" \
	    --eval "(byte-compile-file \"$$f\")" || { rm -f *.elc; exit 1; }; \
	done
	@rm -f *.elc

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
