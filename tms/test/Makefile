include ../../etc/site.make
TTY ?= $(shell tty)

.PHONEY: test migrate

UNITTESTS = $(shell find unit -name "*.cls" -printf "%p,")
SCRIPTS = $(wildcard tests.*)

test: migrate db/cache/all.pf
	@for script in $(SCRIPTS); do \
	    WORK_DIR=$(WORK_DIR) ./$$script || exit 2; \
	done
	@PROPATH=$(WORK_DIR)/tms:.:$(WORK_DIR)/tools \
	$(MPRO) -b -p gearbox/unit/run_tests.r \
		-clientlog /dev/null $(FORMAT_PARAMS) \
		-pf db/cache/all.pf -param out=$(TTY),$(UNITTESTS)

migrate:
	$(MAKE) -C db create migrate DB_USER=""

db/cache/all.pf:
	$(MAKE) -C $(@D) $(@F)
