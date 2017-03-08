#!/bin/bash

TMSPATH=/apps/xfera
RPATH=/tmsapps
TERM=xterm

export RPATH
PROPATH=$RPATH,$TMSPATH

for dir in tms tools tools/stompAdapter
do
   PROPATH=$PROPATH,$TMSPATH/$dir
done

export TMSPATH
export PROPATH
export TERM

die () {
    echo >&2 "$@"
    exit 1
}

[ -n "$1" ] || die "Expected dumpid as parameter"

mpro -b -q -pf /apps/xfera/tms/pf/batch.pf -p HPD/hpd_filedump_batch.p -clientlog /apps/yoigo/var/log/hpd_filedump_${1}.log -param ${1}
