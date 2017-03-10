#!/bin/bash

die () {
    echo >&2 "$@"
    exit 1
}

[ -n "$1" ] || die "Expected dumpid as parameter"

if [ -z "$2" ]; then
   tenant = "yoigo"
else
   tenant = $2

[ "$tenant" != "super" ] || die "Super tenant is not allowed"

echo "Using tenant $tenant"

cd "${0%/*}"
cd ../
pike batch -- HPD/hpd_filedump_batch common ordercanal mobile counter star reratelog  -clientlog ../var/log/hpd_filedump_${1}_${tenant}.log -param ${1}
