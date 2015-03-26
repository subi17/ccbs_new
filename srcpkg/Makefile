include ../etc/site.make

GEARBOX_VERSION := $(firstword $(wildcard gearbox-*.tar))
FCGI_VERSIONS   := $(shell for ii in $(RPCS); do readlink ../rpc/$$ii/lib; done)
FCGI_VERSIONS   := $(subst ../,,$(FCGI_VERSIONS))
WEBSERVER_USER  ?= $(shell id -nu)
PRODIGY_VERSION := $(firstword $(wildcard prodigy-$(NAME)-*.tar))
SIMNET_VERSION  := $(firstword $(wildcard simnet-*.tar))

initialize: gearbox $(MODULES)

gearbox: $(GEARBOX_VERSION)
	@tar xf $<; \
	PREFIX=$(WORK_DIR)/tools DLC=$(DLC) $(MAKE) -C gearbox install; \
	rm -r gearbox

rpc: $(FCGI_VERSIONS:.tar=)

fcgi_agent-%: fcgi_agent-%.tar
	@tar xf $<; \
	versionrpc=$$(for jj in $(RPCS); do if test "`readlink ../rpc/$$jj/lib`" == "../$@"; then echo -n "$$jj "; fi; done); \
	$(MAKE) -C $@ install PREFIX=$(WORK_DIR)/rpc RPCS="$$versionrpc" ALLRPCS="$(RPCS)" CUSTOMER=$(NAME) RUN_AS=$(WEBSERVER_USER) STATE_DIR=$(WORK_DIR)/var CCBS=1
	@rm -r $@

ifeq "$(ENVIRONMENT)" "production"
# That file exists already, if this is a distribution archive
prodigy: ../prodigy/bin/prodigy

ifdef PRODIGY_VERSION
../prodigy/bin/prodigy:
	$(MAKE) $(PRODIGY_VERSION:.tar=)
endif

else
prodigy: $(PRODIGY_VERSION:.tar=) $(SIMNET_VERSION:.tar=)
endif

prodigy-$(NAME)-%: prodigy-$(NAME)-%.tar
	@tar xf $<
	$(MAKE) -C $(<:.tar=) install PREFIX=$(WORK_DIR)/prodigy CUSTOMER=$(NAME) STATEDIR=$(WORK_DIR)/var CCBS=1 || \
	{ echo; \
	  echo "*******************************************"; \
	  echo "******** UNABLE TO INSTALL PRODIGY ********"; \
	  echo "*******************************************"; \
	  echo; }
	@rm -r $@

simnet-%: simnet-%.tar
	tar xf $<; cd $@; \
	python ./setup.py install --prefix=$(WORK_DIR)/prodigy
	@rm -r $@
 
tms:
