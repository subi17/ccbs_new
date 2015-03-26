include ../../../etc/site.make
TMSCODES_FILE := $(DB_DIR)/../fixtures/tmscodes.yaml

help:
	@echo "To search for TMS codes for TABLE.FIELD type 'make TABLE.FIELD'"

ifeq "$(wildcard $(TMSCODES_FILE))" ""
  $(error "Code fixture $(TMSCODES_FILE) not found")
endif

%:
	@echo "TMS codes for $@"
	@python magic.py $(TMSCODES_FILE) $(basename $@) $(subst .,,$(suffix $@))
