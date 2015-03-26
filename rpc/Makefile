include ../etc/site.make

.PHONEY: test rundaemons stopdaemons build help newrpc compile
SERVICES := $(wildcard $(WORK_DIR)/etc/httpd/rpc-*.conf)
SERVICES := $(patsubst $(WORK_DIR)/etc/httpd/rpc-%.conf,%,$(SERVICES))

test::
	for dir in $(RPCS); do \
	    $(MAKE) -C $$dir test || exit 2; \
	done


rundaemons::
ifneq "$(WEBSERVER_USER)" ""
	@if ! sudo -u $(WEBSERVER_USER) id; then \
	    echo "You cannot sudo to $(WEBSERVER_USER) - aborting!" >&2; \
	    exit 2; \
	fi
	@for subdir in log run; do \
	    perms=$$(stat -c %A $(WORK_DIR)/var/$$subdir); \
	    if test $${perms:8:1} == "w"; then \
		echo "Directory var/$$subdir is world-writable - aborting" >&2; \
		exit 2; \
	    fi; \
	    if ! sudo -u $(WEBSERVER_USER) \
			touch $(WORK_DIR)/var/$$subdir/.writetest; then \
		echo "User $(WEBSERVER_USER) has no write permissions on var/$$subdir - aborting" >&2; \
		exit 2; \
	    else \
		rm -f $(WORK_DIR)/var/$$subdir/.writetest; \
	    fi; \
	done
endif
	for service in $(SERVICES); do \
	    if test -f $(WORK_DIR)/var/run/lighttpd-$$service.pid; then \
	        echo "Lighttpd for rpc $$service already running" >&2; \
	    else \
	        DLC=$(DLC) lighttpd -f $(WORK_DIR)/etc/httpd/rpc-$$service.conf; \
	    fi; \
	done

stopdaemons:
	for service in $(SERVICES); do \
	    if test -f $(WORK_DIR)/var/run/lighttpd-$$service.pid; then \
	        kill `cat $(WORK_DIR)/var/run/lighttpd-$$service.pid`; \
	    else \
	        echo "Lighttpd for rpc $$service not running" >&2; \
	    fi; \
	done

compile::
	for dir in $(RPCS); do \
	    $(MAKE) -C $$dir compile BUILD_DIR=$(BUILD_DIR); \
	done

build::
	for dir in $(RPCS); do \
	    $(MAKE) -C $$dir build BUILD_DIR=$(BUILD_DIR) DB_DIR=$(DB_DIR); \
	done

newrpc::
	@if test -z "$(RPC)"; then \
	    echo "Syntax: make newrpc RPC=<name>"; \
	else \
	    mkdir -p $(RPC)/rpcmethods $(RPC)/test; \
	    echo "include ../.ModuleMakefile" >$(RPC)/Makefile; \
	    sed 's@\(include ../..\)@\1/../..@' .ModuleTestMakefile >$(RPC)/test; \
	    echo "Done - don't forget to add this name to etc/config.make"; \
	fi

console::
	@proxies=""; \
	for conffile in $(WORK_DIR)/etc/httpd/rpc-*.conf; do \
	    port=$$($(SED) -n 's/^\s*server\.port\s*=\s*\([0-9]\+\)\s*$$/\1/;T;p' $$conffile); \
	    proxies="$${proxies}$$($(SED) -n 's@^ *"/\([-_a-z0-9A-Z]\+\)" *=> *(.*@\1=_sp('"'"'http://localhost:'$$port'/\1/'"'"');@;T;p' $$conffile)"; \
	done; \
	python -i -c "from xmlrpclib import ServerProxy as _sp;$${proxies}print('Use %s as server objects' % [x for x in dir() if not x.startswith('_')])"

help::
	@echo "This makefile should be called only by Main.make and mainly"; \
	echo "calls only make on subdirectories."; \
	echo "You can use 'newrpc' to make a rpc-dir template, though."; \
	echo "On production the 'console' target helps in smoke testing."
