include ../../../etc/site.make

.PHONEY: load

CALL_PARAMS	:= -b -p gearbox/fixtures/load_fixtures.r $(FORMAT_PARAMS) \
		   -pf $(WORK_DIR)/db/progress/store/cache/all.pf \
		   -param fix_dir=$(CURDIR)

load: $(WORK_DIR)/db/progress/store/cache/all.pf
	@echo "Loading all fixtures ..."
	PROPATH=$(WORK_DIR):$(WORK_DIR)/tools \
	$(MPRO) $(CALL_PARAMS)

load_%:: %.yaml $(WORK_DIR)/db/progress/store/cache/all.pf
	@echo "Loading fixture $<"
	PROPATH=$(WORK_DIR):$(WORK_DIR)/tools \
	$(MPRO) $(CALL_PARAMS),fixtures=$<

$(WORK_DIR)/db/progress/store/cache/all.pf:
	$(MAKE) -C $(@D) $(@F)
