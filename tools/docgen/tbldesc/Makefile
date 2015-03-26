include ../../../etc/site.make

help:
	@echo "To search for table X just type 'make X'"

%:
	@rm -f .out
	@FOUND=0; \
	for db in $(DATABASES); do \
	    $(MPRO) -db $(DB_DIR)/$$db -b -p protabledesc.p \
		    -param .out,$@ || exit 2; \
	    if test -s .out; then \
		FOUND=1; \
		sed 's/ for $@$$/ for '$$db'.$@/' .out; \
	    fi; \
	    rm -f .out; \
	done; \
	test $$FOUND -eq 1
