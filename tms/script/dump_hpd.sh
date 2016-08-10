#!/bin/bash

TMSPATH=/apps/xfera
RPATH=/tmsapps
TERM=xterm

export RPATH
PROPATH=$RPATH,$TMSPATH

for dir in tms/Ar tms/Func tms/Gwy tms/Help tms/Inv tms/Mc tms/Mf tms/Mm tms/Mnp tms/Rate tms/Syst tms/triggers tms/templates tools tools/stompAdapter
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

mpro -b -q -pf /apps/xfera/tms/pf/batch.pf -p hpd_filedump_batch.p -clientlog /apps/yoigo/var/log/hpd_filedump_${1}.log -param ${1}
