include ../../../etc/site.make

# How this works:
#   This makefile includes a file .dependencies, which does not exist
#   so, it produces it with the big target below. That target loops through
#   all migration .py files. Those that have a number less than the requested
#   VERSION and for which no .done file exists, an entry is added to a list.
#   Also for those that are above the requested version, but a .done file
#   exists, a down_$file entry is PREpended to the list.
#   Finally that list is written into the .dependencies file, so that it
#   defines the .dependencies for "migrate". So when finally the migrate target
#   is executed, each lower-non-exist migration will execute the $file.done
#   target and each higher-but-exists migration will execute the down_$file


.PHONEY: migrate status all validate_cache dbs_started delete_self

DB_DIR          ?= $(WORK_DIR)/db/progress/store
VERSION		?= 9999

all: migrate

.dependencies:
	@list=""; \
	shopt -s nullglob; \
	ii=1; \
	while test $$ii -le $(VERSION); do \
	    jj=$$(printf "%04d" $$ii); \
	    files="$$(echo $${jj}_*.py)"; \
	    if test -n "$$files"; then \
	        for file in $$files; do \
		    if ! test -f $(DB_DIR)/migrations/$$file.done; then \
		        list="$$list $(DB_DIR)/migrations/$$file.done"; \
		    fi; \
	        done; \
	    else \
		break; \
	    fi; \
	    ii=$$(($$ii + 1)); \
	done; \
	while :; do \
	    jj=$$(printf "%04d" $$ii); \
	    files="$$(echo $${jj}_*.py)"; \
	    if test -n "$$files"; then \
	        for file in $$files; do \
		    if test -f $(DB_DIR)/migrations/$$file.done; then \
		        list="down_$$file $$list"; \
		    fi; \
	        done; \
	    else \
		break; \
	    fi; \
	    ii=$$(($$ii + 1)); \
	done; \
	for file in $(DB_DIR)/migrations/*.py; do \
	    if ! test -f $$(basename $$file); then \
	        list="down_$$(basename $$file) $$list"; \
	    fi; \
	done; \
	echo "migrate: delete_self dbs_started validate_cache $$list" > $@; \
	echo "	$(MAKE) -C $(DB_DIR)/cache" >> $@

delete_self:
	rm .dependencies

ifeq '' '$(DBNUM)' # Normal: include "migrate" target from .dependencies
include .dependencies
else 		   # Migration for a single monthly database
		   # Only called from the store-Makefile
monthly: monthly_$(DBNAME).py
	ENVIRONMENT=$(ENVIRONMENT) PYTHONPATH=$(WORK_DIR)/tools \
	python $(WORK_DIR)/tools/gearbox/migrations/mig2df.pyc monthly_$(DBNAME).py up $(DB_DIR) > monthly_$(DBNAME).df
	echo "ROUTINE-LEVEL ON ERROR UNDO, THROW." > migrate.p; \
	echo "SESSION:SUPPRESS-WARNINGS = TRUE." >> migrate.p; \
	echo "RUN prodict/load_df('monthly_$(DBNAME).df,,NEW OBJECTS')." >> migrate.p
	$(MPRO) -db $(DB_DIR)/$(DBNAME)$(DBNUM) $(FORMAT_PARAMS) -b -p migrate.p | cat; \
	if test -f $(DBNAME)$(DBNUM).e; then \
	    cat $(DBNAME)$(DBNUM).e; rm $(DBNAME)$(DBNUM).e; exit 2; \
	fi; \
	rm -f $(<:.py=.df) migrate.p
endif

define ASSERT_CACHE_WRITABLE
@if ! test -w $(DB_DIR)/migrations; then \
    echo "Cache directory $(DB_DIR)/migrations is not writable - aborting" >&2; \
    exit 2; \
fi
endef

define MIGRATE_COMMON
@$(MAKE) -C $(DB_DIR)/cache update_a2t
@ENVIRONMENT=$(ENVIRONMENT) PYTHONPATH=$(WORK_DIR)/tools \
python $(WORK_DIR)/tools/gearbox/migrations/mig2df.pyc $< $1 $(DB_DIR) > $(<:.py=.df)
@echo "SESSION:SUPPRESS-WARNINGS = TRUE." > migrate.p; \
echo "RUN prodict/load_df('$(<:.py=.df),,NEW OBJECTS')." >> migrate.p
@changed_db=$$(sed -n 's/^ *database *= *["'\'']\(.*\)["'\''] */\1/;T;p' $<); \
$(MPRO) -db $(DB_DIR)/$$changed_db $(FORMAT_PARAMS) -b -p migrate.p | cat; \
if test -f $$changed_db.e; then \
    cat $$changed_db.e; rm $$changed_db.e; exit 2; \
fi; \
rm -f $(<:.py=.df) migrate.p $(DB_DIR)/cache/$$changed_db.{df,a2t}
endef

$(DB_DIR)/migrations/%.py.done: %.py
	$(call ASSERT_CACHE_WRITABLE)
ifdef DRY_RUN
	ENVIRONMENT=$(ENVIRONMENT) PYTHONPATH=$(WORK_DIR)/tools \
	python $(WORK_DIR)/tools/gearbox/migrations/mig2df.pyc $< up $(DB_DIR)
else
	$(call MIGRATE_COMMON,up)
	@date "+%FT%T $$USER up  " | cat ${@:.done=.gone} - > $@ 2>/dev/null; \
	cp $< $(DB_DIR)/migrations; \
	rm -f $(DB_DIR)/migrations/$<.gone
endif

down_%.py:: $(DB_DIR)/migrations/%.py
	$(call ASSERT_CACHE_WRITABLE)
ifdef DRY_RUN
	@ENVIRONMENT=$(ENVIRONMENT) PYTHONPATH=$(WORK_DIR)/tools \
	python $(WORK_DIR)/tools/gearbox/migrations/mig2df.pyc $< down $(DB_DIR)
else
	$(call MIGRATE_COMMON,down)
	@date "+%FT%T $$USER down" >> $<.done; \
	rm -f $<; \
	mv $<.done $<.gone
endif

dry_run:
	@$(MAKE) -C $(DB_DIR)/cache update_a2t; \
	$(MAKE) migrate DRY_RUN=1

validate_cache:
	$(MAKE) -C $(DB_DIR)/cache validate
dbs_started:
	$(MAKE) -C $(DB_DIR) start

status:
	@shopt -s nullglob; \
	for mig in *.py; do \
	    if test -f $(DB_DIR)/migrations/$$mig.done; then \
		printf "%-40s: done\n" $$mig; \
	    else \
		printf "%-40s: not yet\n" $$mig; \
	    fi; \
	done


history:
	@cd $(DB_DIR)/migrations/; \
	grep . *.py.?one | sed 's/\([0-9]*\)_.*one:\(.*\)/\2 \1/' | sort


help:
	@echo "Targets (default: migrate):"; \
	echo "  migrate	:: With optional parameter VERSION: migrate"; \
	echo "  status	:: Show all migrations with status: done/not yet"; \
	echo "  history	:: Show timestamp and user of all migrate-actions"
