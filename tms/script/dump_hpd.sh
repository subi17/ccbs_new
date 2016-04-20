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

IFS=","
database=""
for db in $4
do
   database=$database" -db /db1/"$db"/"$db
done

IFS=" "

export TMSPATH
export PROPATH
export TERM

die () {
    echo >&2 "$@"
    exit 1
}

[ -n "$1" ] || die "Expected dumpid as parameter"

dumpid=$1


pid_file='../var/run/d-'$daemon$instance'.pid'

! [ -e "$pid_file" ] || die "The " $daemon$instance " is already running"
    
echo "Starting " $daemon$instance "... "

mpro -b -q -pf /apps/xfera/tms.pf -p HPD/hpd_filedump_batch.p -clientlog /apps/yoigo/var/log/hpd_filedump_${1}.log -param ${1}

# nohup mpro -b -p gearbox/daemons/run_daemon.p -clientlog ../var/log/d-$daemon${instance}.log -param ${daemon},${instance},../var/run -T ../var/tmp -logginglevel $3 $database -h 15 -TB 31 -TM 32 -s 1024 -d dmy -e 100 -l 1000 -yy 1980 -rand 2 -tmpbsize 8 -Bt 1024 -numsep 44 -numdec 46 &>/dev/null &
