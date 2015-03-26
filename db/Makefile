include ../etc/site.make

.PHONEY: initialize

RDBMSSES := $(filter-out Makefile,$(wildcard *))

all:
	@echo "This should only be called internally"

initialize: $(RDBMSSES)

# This block is generated (by the foreach below) for each rdbms with
#   1 = mysql			| progress
#   2 = $(DB_mysql_PARTITION)	| $(DB_progress_PARTITION)
define alldeps
ifneq '' '$2'
$1: $2 $(addsuffix .db,$(addprefix $1/store/,$(DATABASES))) $1.init

$1/store/%.db: $2/%.db
	ln -s $$< $1/store; \
	ln -s $$(<:.db=.lk) $1/store; \
	ln -s $$(<:.db=.lg) $1/store; \
	ln -s cache/$$(@F:.db=.st) $1/store; \
	$(MAKE) -C $1/store/cache $$(@F:.db=.df) $$(@F:.db=.st)

$2/%.db: $2/Makefile
	if ! test -f $$@; then \
	    mkdir $2/migrations &>/dev/null; \
	    $(MAKE) -C $2 $$(@F); \
	fi

$2:
	@echo "DB-Partition $2 does not exist - aborting"; \
	echo "Please set the correct path in etc/site.make"; \
	exit 2

$2/Makefile: $1/store/Makefile
	$(SED) 's@^\(PATH_LEVELS ?= \).*@\1 $(WORK_DIR)@' $$< > $$@

$2/migrations:
	mkdir $2/migrations; \
	shopt -s nullglob; \
	for ii in $1/migrations/*.{py,rb}; do \
	    mig=$$$$(basename $$$$ii); \
	    date "+%FT%T initial up" >$2/migrations/$$$$mig.done; \
	done

$1/store/migrations: $2/migrations
	ln -s $2/migrations $1/store

else
$1: $1.init

$1/store/migrations:
	mkdir $$@
endif
endef

$(foreach P,$(RDBMSSES),$(eval $(call alldeps,$(P),$(DB_$(P)_PARTITION))))

%.init: %/store/migrations
	$(MAKE) -C $(@:.init=)/store create start
ifeq "$(ENVIRONMENT)" "development"
	if test "$(@:.init=)" == "progress"; then \
	    $(MAKE) -C $(@:.init=)/store new_monthlys; \
	fi
	$(MAKE) -C $(@:.init=)/migrations
	$(MAKE) -C $(@:.init=)/fixtures
endif


start: $(addsuffix .start,$(RDBMSSES))
stop: $(addsuffix .stop,$(RDBMSSES))

%.start:
	$(MAKE) -C $(@:.start=)/store start
%.stop:
	$(MAKE) -C $(@:.stop=)/store stop

