include ../../etc/site.make

.PHONEY: test

TEST_DB_PF = $(WORK_DIR)/tms/test/db/cache/all.pf
ACTIONS := $(wildcard *.yaml)

test: $(addprefix test_,$(ACTIONS:.yaml=))

test_%: %.yaml make_commlines.r $(TEST_DB_PF)
	@echo "Validating $(<:.yaml=) commlines"; \
	PROPATH=.:$(WORK_DIR)/tools:$(WORK_DIR)/tms \
	$(MPRO) -b -p make_commlines -pf $(TEST_DB_PF) -param $<,$@.clines; \
	cat $@.clines | ./validate >/dev/null
	@rm $@.clines

make_commlines.r: make_commlines.p $(TEST_DB_PF)
	@echo "Compiling $<"; \
	echo "COMPILE $< SAVE." > compile.p; \
	PROPATH=.:$(WORK_DIR)/tools:$(WORK_DIR)/tms \
	$(MPRO) -pf $(TEST_DB_PF) -b -p compile.p; \
	rm compile.p

$(TEST_DB_PF):
	$(MAKE) -C $(@D) $(@F)
