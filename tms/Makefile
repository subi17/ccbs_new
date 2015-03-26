include ../etc/site.make

# Set this for additional command line args to the compile call
ADDARGS		:=  -s 150
HUGE_PROPATH    := "Mc:Mf:Mm:Ar:Class:Dev:Func:Gwy:Help:Inv:Mnp:Rate:Syst:utilities:xmlrpc:xsd"

# Big change from previous versions: No dependency juggling anymore, as
# we will not keep any .r files in development mode. When you want to have
# the .rs, which happens ONLY for testing and deployment, give a TARGET.

.PHONEY: test rundaemons stopdaemons compile clean build help run
DAEMONS	:= $(patsubst daemons/%.cls,%,$(wildcard daemons/*.cls))
DAEMONS := $(filter-out BaseDaemon,$(DAEMONS))
.PRECIOUS: $(patsubst %,daemons/%.r,$(DAEMONS))
TTY ?= $(shell tty)

NONP_SOURCE := daemons/run_daemon.sh

TARGET	  ?= .
BUILD_DIR ?= ..
COMPFILE  := compile
BOGUS     := $(shell rm -f $(COMPFILE).{r,p})

test::
	$(MAKE) -C test

rundaemons: $(patsubst %,../var/run/d-%.pid,$(DAEMONS))

define RUNDAEMON_COMMON
if test -f $@; then \
    echo "WARNING: Daemon running, but dependency changed: $?" >&2; \
else \
    MPRO="$(MPRO)" DNAME=$(subst d-,,$(@F:.pid=)) \
    WORK_DIR="$(WORK_DIR)" nohup sh daemons/run_daemon.sh &>/dev/null &; \
    sleep 1; test -f $@; \
fi
endef

../var/run/d-%.pid: daemons/%.p ../etc/pf/d-%.pf 
	$(RUNDAEMON_COMMON)
../var/run/d-%.pid: tms.pl ../etc/pf/d-%.pf 
	$(RUNDAEMON_COMMON)

stopdaemons:
	@for daemon in $(DAEMONS); do
	    pidfile="../var/run/d-$$daemon.pid"; \
	    if test -f $$pidfile; then \
		pid=`cat $$pidfile`; \
		echo "Killing $$daemon daemon (pid $$pid)"; \
		kill -2 $$pid; \
		for ii in 1 2 3 2 2; do \
		    sleep $$ii; \
		    if ! ps -p $$pid &>/dev/null; then break; fi; \
		done; \
		if ps -p $$pid &>/dev/null; then \
		    echo "** DAEMON $$daemon seems to hang - aborting"; \
		    exit 2; \
		else \
		    rm $$pidfile; \
		fi; \
	    fi; \
	done


build: $(BUILD_DIR)/tms/tms.pl $(addprefix $(BUILD_DIR)/tms/,$(NONP_SOURCE))

$(BUILD_DIR)/tms/%: %
	@-mkdir -p $(@D)
	cp $< $@

%/tms.pl: r
	@-mkdir $(@D) &>/dev/null
	@cd $<; \
	$(DLC)/bin/prolib tms.pl -create; \
	find . -type f -name "*.r" -printf "%P\000" | \
	    xargs --null $(DLC)/bin/prolib tms.pl -add
	@if test -f $</Help/applhelp.r; then \
	    cd $</Help; \
	    $(DLC)/bin/prolib ../tms.pl -add applhelp.r; \
	fi
	@mv $</tms.pl $@; rm -r $<

r:
	$(MAKE) compile TARGET=$@ || { rm -r $@; exit 2; }

SOURCEDIRS      := $(shell find . -maxdepth 1 -type d)
SOURCEDIRS      := $(subst ./,,$(SOURCEDIRS))
SOURCEDIRS      := $(filter-out . test utilities Work Class,$(SOURCEDIRS))
PARALLELCOMPILE ?= 3
compile: $(DB_DIR)/cache/all.pf
	$(MAKE) -j $(PARALLELCOMPILE) $(addprefix execute_compile_,$(SOURCEDIRS))

compile_%.p: %
	@echo "OUTPUT TO $(TTY) UNBUFFERED." > $@; \
	for file in `find $< -name "*.p" -o -name "*.cls"`; do \
	    mkdir -p $(TARGET)/$$(dirname $$file); \
	    echo "PUT UNFORMATTED '.'." >> $@; \
	    if test "$(TARGET)" == "."; then \
	        echo "COMPILE $$file." >> $@; \
	    else \
	        echo "COMPILE $$file SAVE INTO $(TARGET)." >> $@; \
	    fi; \
	done

execute_compile_%: compile_%.p
	@PROPATH=.:..:$(HUGE_PROPATH) $(MPRO) -pf $(DB_DIR)/cache/all.pf $(ADDARGS) -b -p $<
	

$(DB_DIR)/cache/all.pf:
	$(MAKE) -C $(@D) $(@F)


define COMPILE_COMMON
@echo "Compiling $<"
@mkdir -p $$(dirname $@) &>/dev/null; \
echo "ROUTINE-LEVEL ON ERROR UNDO, THROW." > $(COMPFILE).p; \
if test "$(TARGET)" == "."; then \
    echo "COMPILE $<." >> $(COMPFILE).p; \
else \
    echo "COMPILE $< SAVE INTO $(TARGET)." >> $(COMPFILE).p; \
fi; \
CERROR=0; \
PROPATH=.:..:$(HUGE_PROPATH) $(MPRO) -pf $(DB_DIR)/cache/all.pf $(ADDARGS) -b -p $(COMPFILE).p; \
CERROR=$$?; rm -f $(COMPFILE).{p,r}; \
test $$CERROR -eq 0
endef

$(TARGET)/%.r: %.cls $(DB_DIR)/cache/all.pf
	$(COMPILE_COMMON)
$(TARGET)/%.r: %.p $(DB_DIR)/cache/all.pf
	$(COMPILE_COMMON)

clean:
	rm -f procore* tms.pl


run: $(DB_DIR)/cache/all.pf
	@TERM=xterm PROPATH=.,tms.pl \
	$(MPRO) -pf $(DB_DIR)/cache/all.pf -p Syst/tmslogin

help:
	@echo "Targets (default: test):"; \
	echo "  compile    : Compile all files in TARGET (but not .)"; \
	echo "  test       : Delegate to test subdir"; \
	echo "  *.r        : Compile one file"; \
	echo "  clean      : Remove all compiled files"; \
	echo "  run        : Start the character user interface"; \
	echo "  build      : Compile all files into the BUILD_DIR"; \
	echo "  rundaemons : start all daemon processes"; \
	echo "TARGET: $(TARGET)"; \
	echo "DB_DIR: $(DB_DIR)"; \
	echo "DATABASES: $(DATABASES)"
