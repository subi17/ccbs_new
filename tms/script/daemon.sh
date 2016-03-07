# Generic daemon maintenance script.

if test -z "$DAEMON"; then
  echo "Set DAEMON before sourcing" >&2
  exit 2
fi

DAEMONNAME=$DAEMON$INSTANCE
PIDFILE=../var/run/d-$DAEMONNAME.pid
DIEFILE=../var/run/d-$DAEMONNAME.die

if test -z "$INSTANCE"; then
  INSTANCE=?
fi

if test -z "$DATABASES"
then
  DATABASES="common,ordercanal,mobile,star,counter,reratelog"
else
  DATABASES=${DATABASES//' '/,}
fi

if test -n "$LOGLEVEL"
then
  LOGGINGLEVEL=$LOGLEVEL
else
  LOGGINGLEVEL=1    
fi


umask 002

pid=0
if test -f $PIDFILE; then
  pid=`cat $PIDFILE`
  if ! ps -p $pid >/dev/null; then
    rm $PIDFILE
    pid=0
  fi
fi

case "$1" in
  start)
    if test $pid -ne 0; then
      echo "$DAEMONNAME is already running with pid $pid" >&2
      exit 2
    else
      nohup start_daemon.sh $DAEMON $INSTANCE $LOGGINGLEVEL $DATABASES &>/dev/null &
#      nohup pike daemon $DAEMON $INSTANCE -- $DATABASES $LOGGINGLEVEL $EXTRAPARAM &>/dev/null &
      sleep 1
      if ! test -f $PIDFILE; then
        echo "Failed" >&2
        exit 2
      fi
    fi ;;
  stop)
    if test $pid -eq 0; then
      echo "$DAEMONNAME is not running" >&2
      exit 2
    else
      touch $DIEFILE
      for ii in 1 2 3 3 5 5 7 7 7 7 7 7 5 5 3 2 2; do
        sleep $ii
        if ! ps -p $pid >/dev/null; then
          exit 0
        fi
      done
      rm $DIEFILE
      echo "** $DAEMONNAME still running with pid $pid - try killing it!" >&2
      exit 2
    fi ;;
  status)
    if test $pid -eq 0; then
      echo "$DAEMONNAME is not running"
    else
      echo "$DAEMONNAME is running with pid $pid"
    fi ;;
  *)
    echo "Syntax: $0 [start|stop|status]" >&2
    exit 2;;
esac
