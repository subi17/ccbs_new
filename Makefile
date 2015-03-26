###############################################################################
# Main Makefile for a ccbs instance                                           #
# This file needs not be changed. All configuration ist in the files          #
# etc/site.make and etc/config.make                                           #
###############################################################################

.PHONEY: test rundaemons dist docs help initialize all_migrations_executed
.SUFFIXES:

CONFIG_EXISTS := $(wildcard etc/config.make)
ifeq "$(CONFIG_EXISTS)" ""
$(error "Customize this project before using make!")
endif

include etc/config.make
include etc/site.make
BUILD_DIR := builddir/$(NAME)-$(APPVERSION)
ENV	  := $(if $(filter $(shell uname -n),$(PRODUCTION_SERVERS)),production,development)


test:: initialize
ifneq "$(ENVIRONMENT)" "test"
	@if ! grep "precommit *=.*/test_downmig.hook" .hg/hgrc &>/dev/null; then \
	    echo "Mercurial hook for downmig is not installed"; \
	    exit 2; \
	fi
endif
	@$(MAKE) -C db start
	@for dir in $(MODULES); do \
	    echo "Building $$dir..."; \
	    $(MAKE) -C $$dir compile || exit 2; \
	    echo "Running tests for $$dir..."; \
	    $(MAKE) -C $$dir test || exit 2; \
	done


docs:: initialize
	@$(MAKE) -C docs docs


rundaemons:: initialize
	@for dir in $(MODULES); do \
	    echo "Running daemons for $$dir..."; \
	    $(MAKE) -C $$dir rundaemons; \
	done


###############################################################################
#####################  Distribution: Create archive file ######################
###############################################################################

# To prevent modules or databases from going into the distfile, set the
# RDBMSSES or MODULES variables!
RDBMSSES        ?= $(filter-out db/Makefile,$(wildcard db/*))
RDBMSSES        := $(subst db/,,$(RDBMSSES))
RDBMSSES        := $(addprefix db/,$(RDBMSSES))
EMPTY_DIRS_IN_DIST := var var/log var/run tools db \
                      $(RDBMSSES) $(addsuffix /store,$(RDBMSSES)) \
                      $(addsuffix /store/cache,$(RDBMSSES))
COPIED_DIRS_IN_DIST:= $(addsuffix /migrations,$(RDBMSSES)) \
		      $(addsuffix /fixtures,$(RDBMSSES)) \
		      etc srcpkg docs

dist:: $(NAME)-$(APPVERSION).tar.bz2
%.tar.bz2: %.tar
	@rm -f $@ 
	bzip2 $?
%.tar.gz: %.tar
	@rm -f $@
	gzip $?
$(NAME)-%.tar: $(BUILD_DIR)
	tar -cf $@ --exclude=.hg -C $(<D) $(<F)
	@rm -rf $(BUILD_DIR); \
	rmdir builddir
$(BUILD_DIR): all_migrations_executed initialize
	@rm -rf $(BUILD_DIR)
	mkdir -p $(BUILD_DIR)
	cd $(BUILD_DIR); mkdir -p $(EMPTY_DIRS_IN_DIST)
	$(MAKE) -C docs docs
	-for dir in $(EMPTY_DIRS_IN_DIST); do \
	    cp $$dir/Makefile $(BUILD_DIR)/$$dir 2>/dev/null; \
	done
	for dir in $(COPIED_DIRS_IN_DIST); do \
	    cp -r $$dir $(BUILD_DIR)/$$(dirname $$dir); \
	done
	@rm -f $(BUILD_DIR)/etc/site.make $(BUILD_DIR)/docs/Makefile
	@cp .DeployMakefile $(BUILD_DIR)/Makefile
	for dir in $(MODULES); do \
	    $(MAKE) -C $$dir build BUILD_DIR=$(CURDIR)/$(BUILD_DIR) || exit 2; \
	    cp $$dir/Makefile $(BUILD_DIR)/$$dir; \
	done
all_migrations_executed: initialize
	@shopt -s nullglob; \
	for mig in $(CURDIR)/db/progress/migrations/*.py; do \
	    if ! test -f $(CURDIR)/db/progress/store/migrations/$$(basename $$mig).done; then \
		echo "Migration $$(basename $$mig) pending - aborting" >&2; \
		exit 2; \
	    fi; \
	done


###############################################################################
##########  Initialization: setup a checkout for use on this host  ############
###############################################################################

initialize: .initialized

.initialized:
	$(MAKE) -C srcpkg initialize
	$(MAKE) -C db initialize
	@touch .initialized; \
	echo "Initialization successfully finished"


DLC     ?= /opt/dlc/$(PROVERSION)
etc/site.make:
	@if ! test -d "$(DLC)"; then \
	    echo "No progress found in $(DLC) - aborting" >&2; \
	    exit 2; \
	fi
	@if test "$(PROVERSION)" == "101b"; then \
	    echo "Progress version is too old. Require >= 10.1C - aborting" >&2; \
	    exit 2; \
	fi
	@PV=`sed 's/^OpenEdge Release \(.*\)\.\(.*\) as.*/\1\2/i' $(DLC)/version | tr 'A-Z' 'a-z'`; \
	if test "$(PROVERSION)" != "$$PV"; then \
	    echo "Version $$PV in $(DLC) does not match required $(PROVERSION) - aborting >&2"; \
	    exit 2; \
	fi
	@if python -c 'import sys;sys.exit(sys.version_info[:3]>(2,6,0))'; then \
	    echo "Python version too old. Require >= 2.6 - aborting" >&2; \
	    exit 2; \
	fi
	@echo "# Host specific configuration (not under RC)" > $@; \
	echo ".SUFFIXES:" >> $@; \
	echo "SHELL           := /bin/bash" >> $@; \
	if which gsed &>/dev/null; then \
	   echo "SED             := gsed" >> $@; \
	else \
	    echo "SED             := sed" >> $@; \
	fi; \
	echo "DLC		:= $(DLC)" >> $@; \
	echo "WORK_DIR	:= $(CURDIR)" >> $@; \
	echo "DB_DIR		?= $(CURDIR)/db/progress/store" >> $@; \
	echo "BLOCKSIZE	:= 8" >> $@; \
	echo "ENVIRONMENT	:= $(ENV)" >> $@; \
	echo "DB_USER		?= $(if $(filter production,$(ENV)),progress,)" >> $@; \
	echo "WEBSERVER_USER	:= $(if $(filter production,$(ENV)),httpd,)" >> $@; \
	echo "DB_progress_PARTITION	:= $(if $(filter production,$(ENV)),/data/progress/$(NAME))" >> $@; \
	echo "include $(CURDIR)/etc/config.make" >> $@
	@echo "Template of etc/site.make created. Please adjust!"; \
	exit 2


###############################################################################
###################################  Help  ####################################
###############################################################################

help:
	@echo "Targets (default: test):"; \
	echo "  test       :: compile all sources and run all tests"; \
	echo "  rundaemons :: start all daemon processes"; \
	echo "  docs       :: create documentation in doc subdir"; \
	echo "  dist       :: copy db-metadata, compile into build-dir and"; \
	echo "                create archive - define APPVERSION for this!"; \
	echo "  initialize :: setup the freshly checked out/unarchived dir"; \
	echo; \
	echo "APPVERSION   = $(APPVERSION)"; \
	echo "NAME      = $(NAME)"; \
	echo "MODULES   = $(MODULES)"
