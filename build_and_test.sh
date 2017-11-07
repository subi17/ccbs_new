#!/bin/bash


ERROR=0

echo "" | pike initialize
pike -C db start
pike -C db/progress/store migrate
pike -C tms compile || ERROR=2
pike -C db stop
test $ERROR != 0 && exit $ERROR

pike -C tms/test || ERROR=2
pike -C tms/test/db stop
exit $ERROR
