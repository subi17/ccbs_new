#!/bin/bash

die () {
    echo >&2 "$@"
    exit 1
}

[ -n "$1" ] || die "Expected file as parameter"

tenant="yoigo"

if [ -n "$2" ]; then
   tenant=$2
fi

[ "$tenant" != "super" ] || die "Super tenant is not allowed"

echo "Using tenant $tenant"

pike -C /apps/yoigo/tms mbatch -- ../tms_support/sapc/customers_to_pl common tenant=${tenant} -param ${1}

