include ../etc/site.make

.PHONEY: rundaemons compile build test help
LIBINSTALLDIR := $(firstword $(wildcard lib/python*/site-packages))
PIDFILES := $(addprefix ../var/run/,simnet.pid prodigy.run)

ifeq "$(ENVIRONMENT)" "production"
rundaemons: ../var/run/prodigy.run

../var/run/prodigy.run:
	nohup bin/prodigy ../etc/prodigy.conf &
else
rundaemons: $(PIDFILES)

../var/run/simnet.pid:
	PYTHONPATH=$(LIBINSTALLDIR) \
	nohup bin/ibmxml -p 8020 -f etc/fixtures.json >../var/log/simnet.log 2>&1 & echo $$! > ../var/run/simnet.pid

../var/run/prodigy.run:
	nohup bin/prodigy etc/development.conf &
endif

stopdaemons:
	for pidfile in $(PIDFILES); do \
	    if test -f $$pidfile; then \
	        kill `cat $$pidfile`; \
	        rm $$pidfile; \
	    fi; \
	done

compile:
	@echo "No compilation needed"

build:
	@-mkdir -p $(subst $(WORK_DIR),$(BUILD_DIR),$(CURDIR))/bin
	cp bin/prodigy $(subst $(WORK_DIR),$(BUILD_DIR),$(CURDIR))/bin

test::
	$(MAKE) -C test test

help:
	@echo "Targets (default: simulate):"; \
	echo "  simulate   :: run a local provisioning simulator"; \
	echo "  rundaemons :: start all daemon processes"; \
	echo "  test       :: test is TMS implements correct protocol"; \
	echo "  build      :: copy binary into build_dir"; \
	echo "  simulate   :: run in development mode"
