#!/bin/bash

die () {
    echo >&2 "$@"
    exit 1
}

[ -n "$1" ] || die "Expected dumpid as parameter"

tenant="yoigo"

if [ -n "$2" ]; then
   tenant=$2
fi

[ "$tenant" != "super" ] || die "Super tenant is not allowed"

echo "Using tenant $tenant"
cd "${0%/*}"
cd ../
pike batch -- HPD/hpd_filedump_batch common ordercanal mobile counter star reratelog -clientlog ../var/log/hpd_filedump_${1}_${tenant}.log -param ${1} tenant=${tenant} umask=0000

echo "Please check log file hpd_filedump_${1}_${tenant}.log"
