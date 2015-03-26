#!/bin/bash

check () {
   TEMP=`ps -ef | grep "$1" | grep -v grep`

   if [ $? == 1 ]; then
      $1
   else
      echo "This screen is already running"
   fi
}

PROCFG=/opt/dlc/progress.cfg;export PROCFG

if test $USER = 'root'
   then case "$1" in
            online)
            check "screen -S MO -c /apps/tms/screen/screen.mo"
            ;;

#            mnp)
#            check "screen -S MNP -c /apps/tms/screen/screen.mnp"
#            ;;
            
            mnpnc)
            check "screen -S MNP -c /apps/tms/screen/screen.mnpnc"
            ;;

#            smsc)
#            check "screen -S SMSC -c /apps/tms/screen/screen.smsc"
#            ;;

            topup)
            check "screen -S TOPUP -c /apps/tms/screen/screen.topup"
            ;;

            sog)
            check "screen -S SOG -c /apps/tms/screen/screen.sog"
            ;;

            order)
            check "screen -S ORDERHANDLER -c /apps/tms/screen/screen.order"
            ;;

            area)
            check "screen -S AREASTATUS -c /apps/tms/screen/screen.area"
            ;;

            ca)
            check "screen -S CALLALARM -c /apps/tms/screen/screen.ca"
            ;;

#            roam)
#            check "screen -S ROAMCDR -c /apps/tms/screen/screen.roam"
#            ;;

            request)
            check "screen -S REQUEST -c /apps/tms/screen/screen.request"
            ;;

#            trans)
#            check "screen -S TRANS -c /apps/tms/screen/screen.trans"
#            ;;

            counter)
            check "screen -S COUNTER -c /apps/tms/screen/screen.count"
            ;;
            
            roisend)
            check "screen -S ROISEND -c /apps/tms/screen/screen.roisend"
            ;;
            
            iir)
            check "screen -S IIR_TRIGGERS -c /apps/tms/screen/screen.iir"
            ;;

            edr)
            check "screen -S EDR -c /apps/tms/screen/screen.edr"
            ;;

            replog)
            check "screen -S REPLOG -c /apps/tms/screen/screen.replog"
            ;;

            dbup)
            read -sp 'Are you sure you want to start database servers (Y/N)? ' ok
            if test $ok = 'Y'
               then screen -S DBUP -c /apps/tms/screen/screen.dbup
               else echo .
                    echo 'You pressed' $ok 'so database servers are not started.'
            fi
            ;;

            *)
            echo "Usage: `basename $0` {online|topup|sog|order|area|ca|request|counter|roisend|dbup|mnpnc|iir|edr|replog}"
            ;;
        esac
   else echo $USER 'you do not have enough priviledges (try using sudo -s instead of sudo su)'
fi

