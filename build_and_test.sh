#!/bin/bash

cat >etc/site.make <<end-of-site_make
# Host specific configuration (not under RC)
.SUFFIXES:
SHELL           := /bin/bash
SED             := sed
DLC             := /opt/dlc/101c
WORK_DIR        := $PWD
DB_DIR          ?= $PWD/db/progress/store
BLOCKSIZE       := 8
ENVIRONMENT     := test
DB_USER         ?= 
WEBSERVER_USER  := 
DB_progress_PARTITION   := 
DB_mysql_PARTITION      := 
include $PWD/etc/config.make
end-of-site_make

ERROR=0

make initialize
make -C db/progress/store migrate

make test || ERROR=2

make -C db stop
make -C tms/test/db stop

exit $ERROR
