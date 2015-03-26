include ../etc/site.make

DATABASES       := $(wildcard $DBDIR/*.db)
DATABASES       := $(patsubst $DBDIR/%.db,%,$(DATABASES))

.PHONEY: docs
.PRECIOUS: $(patsubst %,$(DB_DIR)/cache/%.df,$(DATABASES))

ROBODOC_MODULES	:= $(MODULES) tools

docs: $(ROBODOC_MODULES) $(addprefix db/,$(DATABASES)) db/menutree.html


$(ROBODOC_MODULES)::
	cd ..; \
	$(SED) -e 's/@MODULE\@/$@/g' etc/robodoc.rc > robodoc.rc; \
	../../temp/document/robodoc/bin/robodoc; \
	rm robodoc.rc


db/%:: $(DB_DIR)/cache/%.df
	@-mkdir -p $@
	@echo "TODO: generate docs from database.df file"


$(DB_DIR)/cache/%.df:
	$(MAKE) -C $(@D) $(@F)

db/menutree.html:
	@-mkdir $(@D)
	PROPATH=../tools,../tms HOST="$(shell uname -n)" \
	$(MPRO) -pf $(DB_DIR)/cache/all.pf -b -p docgen/menutree/menu2luna.p -param http://foo,$@

