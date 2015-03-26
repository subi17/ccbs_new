PATH_LEVELS ?= ../../../..
include $(PATH_LEVELS)/etc/site.make

.PHONEY: update update_a2t validate

update: $(addsuffix .df,$(DATABASES)) $(addsuffix .st,$(DATABASES)) $(addsuffix .a2t,$(DATABASES)) dict tabledict all.pf

update_a2t: $(addsuffix .a2t,$(DATABASES))

validate: $(addsuffix .df.diff,$(DATABASES))
	@for df in $?; do \
	    if ! diff $${df%.diff} $$df; then\
		echo "!!! ALERT ALERT ALERT !!!" >&2; \
		echo "DB structure of $${df/.df.diff/} was changed manually - aborting" >&2; \
		exit 2; \
	    fi; \
	done
	@rm $?

all.pf:
	echo "-h $(words $(DATABASES) $(MONTHLY_DBS))" > $@; \
	for db in $(DATABASES); do \
	    echo "-db $(CURDIR:/cache=)/$$db" >> $@; \
	done
	for db in $(MONTHLY_DBS); do \
	    num=$$((`cat next.$$db` - 1)); \
	    num=`printf "%03d" $$num`; \
	    echo "-db $(CURDIR:/cache=)/$$db$$num -ld $$db" >> $@; \
	done

%.df.diff %.df: ../%.lk
	@echo "RUN prodict/dump_df ('ALL', '$@', 'UTF-8')." > dump.p; \
	$(MPRO) -b -p dump.p -db $(<:.lk=)
	@rm -f dump.p

../%.lk:
	$(MAKE) -C $(@D) $(@F)

%.a2t:
	@$(DLC)/bin/proutil ../$(@:.a2t=) -C tabanalys | \
	$(SED) -n 's/^RECORD BLOCK SUMMARY FOR AREA "\(.*\)" : [0-9]*/\1,/;T 1;x;b;:1;s/^PUB\.\([^ ]*\) .*/\1;/;T 2;H;b;:2;/^$$/{x;/^$$/b;s/\n//g;s/;$$//;p}' > $@

%.st:
	$(DLC)/bin/prostrct list ../$(@:.st=) $@ >/dev/null

tabledict: $(addsuffix .df,$(DATABASES))
	@for file in $(DATABASES); do \
	    $(SED) -n -e 's/ADD TABLE "\(.*\)"/\1;'$$file'/;T;p' $$file.df; \
	done > $@

dict: $(addsuffix .df,$(DATABASES))
	@for file in $(DATABASES); do \
	    $(SED) -n -e 's/ADD FIELD "\(.*\)" OF "\(.*\)" AS \([^ ]*\)/\2.\1;\3;/;T 1;x;d' -e ':1;s/ *\(FORMAT .*\)/\1/;T;H;x;s/ *\n//;p' $$file.df; \
	done > $@
