#!/bin/bash

check () {
   mkdir -p /var/run/tms
   TEMP=`ps -ef | grep "$1" | grep _progres`
   DEXIST=$?
   if [ $DEXIST -gt 0 ]; then
      $2
   else
      echo "This daemon is already running"
   fi
}

PROCFG=/opt/dlc/progress.cfg;export PROCFG
export display_banner=no
if test $USER = 'root'
   then case "$1" in
            funcrun)
            check funcrun_daemon "/opt/local/bin/xfear -bg_batch funcrun_daemon.p tms.pf"
            ;;

            *)
            echo "Usage: `basename $0` {funcrun}"
            ;;
        esac
   else echo $USER 'you do not have enough priviledges (try using sudo -s instead of sudo su)'
fi

