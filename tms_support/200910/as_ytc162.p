{commpaa.i}
gcBrand = "1".
katun = "YTC-161".
{msreqfunc.i}

def stream loki.
output stream loki to /apps/snet/200910/as_ytc162.log append.
DEFINE VARIABLE i AS INTEGER NO-UNDO. 
FOR EACH msrequest where
   msrequest.brand = "1" and
   msrequest.reqtype = 13 and
   msrequest.reqstatus = 3 and
   index(msrequest.memo,"61150") > 0 NO-LOCK:
   find mobsub where mobsub.msseq = msrequest.msseq NO-LOCK NO-error.
   IF not avail mobsub then do:
      put stream loki unformatted msrequest.msrequest skip.
      fReqStatus(0,"").
   end.
END.
