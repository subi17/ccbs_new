def stream slog.
output stream slog to /apps/snet/200911/as_yot307.log.
FOR EACH mobsub where
  custnum = 647386 EXCLUSIVE-LOCK:
  put stream slog unformatted mobsub.msseq "|" mobsub.idcode "|4444" skip.
  mobsub.idcode  = "4444".
end.
