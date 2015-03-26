PATH_LEVELS ?= ../../..
include $(PATH_LEVELS)/etc/site.make

.PHONEY: create start stop restart help status migrate
.PRECIOUS: $(addsuffix .st,$(DATABASES)) $(addsuffix .st,$(MONTHLY_DBS)) $(addprefix cache/next.,$(MONTHLY_DBS))

migrate: start
	$(MAKE) -C $(WORK_DIR)/db/progress/migrations migrate DB_DIR=$(CURDIR)

create: $(addsuffix .db, $(DATABASES))

start: $(addsuffix .lk, $(DATABASES))
	@for mcdr in cache/next.*; do \
	    num=$$(printf "%03d" $$(($$(cat $$mcdr) - 1))); \
	    mcdr=$${mcdr#cache/next.}; \
	    $(MAKE) $$mcdr$$num.lk; \
	done

ifeq "$(ENVIRONMENT)" "production"
%.st: $(WORK_DIR)/db/progress/migrations/%.st
	$(SED) -e 's@BASEDIR@$(CURDIR)@' $< > $@
else
%.st:
	@echo "b $(CURDIR)/bi" > $@; \
	echo "#" >> $@; \
	echo 'd "Schema Area" $(CURDIR)/db' >> $@; \
	echo "#" >> $@
	@for size in 64 128 256; do \
	    echo "d \"Sta_Data_$$size\",$$size $(CURDIR)/db" >> $@; \
	    echo "#" >> $@; \
	done
	@echo 'd "Sta_Index_64",64 $(CURDIR)/ix' >> $@; \
	echo "#" >> $@
endif


%.db: %.st
	@if ! test -f $@; then \
	mkdir db ix bi &>/dev/null; \
	$(DLC)/bin/prostrct create ${@:.db=} -blocksize $$(($(BLOCKSIZE) * 1024)); \
	for emptydb in $(DLC)/prolang/{utf,eng}/empty$(BLOCKSIZE).db " "; do \
	    if test -f $$emptydb; then break; fi; \
	done; \
	$(DLC)/bin/procopy -s $$emptydb ${@:.db=}; \
	echo -e ".\nPSC\ncpstream=\n.\n0000000002" >cache/${@:.db=.df}; \
	rm -f cache/all.pf; \
	rm $<; ln -s cache/$< $<; $(MAKE) -C cache $<; \
	fi

%.lk:
	@echo "Starting database $(@:.lk=)"; \
        command="$(DLC)/bin/proserve $(@:.lk=)"; \
        test -f $(WORK_DIR)/etc/pf/$(@:.lk=.pf) && command="$$command -pf $(WORK_DIR)/etc/pf/$(@:.lk=.pf)"; \
	if test -z "$(DB_USER)"; then \
	    $$command; \
	else \
	    sudo $(DB_USER) $$command; \
	fi


new_monthlys: $(addprefix monthly_,$(MONTHLY_DBS))
	$(MAKE) -C cache all.pf

monthly_%: %.st cache/next.%
	@-mkdir db ix bi &>/dev/null
	dbname=$(subst monthly_,,$@); \
	next_num=`cat cache/next.$$dbname`; \
	cp $< $$dbname$$next_num.st; \
	$(DLC)/bin/prostrct create $$dbname$$next_num -blocksize $$(($(BLOCKSIZE) * 1024)); \
	for emptydb in $(DLC)/prolang/{utf,eng}/empty$(BLOCKSIZE).db " "; do \
	    if test -f $$emptydb; then break; fi; \
	done; \
	$(DLC)/bin/procopy -s $$emptydb $$dbname$$next_num; \
	$(MAKE) -C cache $$dbname$$next_num.a2t; \
	mv cache/$$dbname$$next_num.a2t cache/$$dbname.a2t; \
	$(MAKE) $$dbname$$next_num.lk; \
	$(MAKE) -C $(WORK_DIR)/db/progress/migrations monthly DB_DIR=$(CURDIR) DBNAME=$$dbname DBNUM=$$next_num; \
	printf "%03d" $$(($$next_num + 1)) > cache/next.$$dbname
	rm -f cache/all.pf

$(addprefix cache/next.,$(MONTHLY_DBS)):
	echo "000" > $@

stop:
	@for db in $(DATABASES); do \
	    echo "Stopping database $$db"; \
	    if ! test -f $$db.lk; then \
		echo "Warning: Database $$db is not running"; \
		continue; \
	    fi; \
	    if test -z "$(DB_USER)"; then \
		$(DLC)/bin/proshut -by $$db; \
	    else \
		sudo $(DB_USER) $(DLC)/bin/proshut -by $$db; \
	    fi; \
	done
	@shopt -s nullglob; \
	for db in *.lk; do \
	    db=$${db%.lk}; \
	    echo "Also stopping $$db"; \
	    if test -z "$(DB_USER)"; then \
		$(DLC)/bin/proshut -by $$db; \
	    else \
		sudo $(DB_USER) $(DLC)/bin/proshut -by $$db; \
	    fi; \
	done


status:
	@for db in $(DATABASES); do \
	    if test -f $$db.lk; then \
	        echo "Databases $$db is running"; \
	    else \
	        echo "Databases $$db is offline"; \
	    fi; \
	done


restart:
	$(MAKE) stop
	$(MAKE) start


help:
	@echo "Targets (default: migrate): "; \
	echo "  start	Start all databases"; \
	echo "  stop	Stop all databases"; \
	echo "  restart	Stop and start all databases"; \
	echo "  status	Show running/not running for all databases"; \
	echo; \
	echo "DATABASES: $(DATABASES)"; \
	echo "DB_USER: $(DB_USER)"
