######################################
# Makefile for nurarihyon
######################################

LISP             = sbcl
LISP_OPTIONS     = --noinform --load
LISP_SYSTEM_DIR  = $(HOME)/.sbcl/systems
LISP_SITE_DIR    = $(HOME)/.sbcl/site
NURARIHYON_ASD_PATH   = $(LISP_SYSTEM_DIR)/nurarihyon.asd
NURARIHYON_TARGET_DIR = $(LISP_SITE_DIR)/nurarihyon
NURARIHYON_SRC_DIR    = $(PWD)/src

LN=ln -sf
RM=rm -f

all: install-src install-asd

install-src:
	$(LN) $(NURARIHYON_SRC_DIR) $(NURARIHYON_TARGET_DIR)

install-asd:
	$(LN) $(NURARIHYON_SRC_DIR)/nurarihyon.asd $(NURARIHYON_ASD_PATH)

clean:
	$(RM) $(NURARIHYON_ASD_PATH) $(NURARIHYON_TARGET_DIR)
	$(RM) src/*fasl

test:
	$(LISP) $(LISP_OPTIONS) $(PWD)/tests/test-nurarihyon.lisp 2>/dev/null
