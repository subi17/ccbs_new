{Syst/commpaa.i}
katun  = "anttis".
gcBrand = "1".
{Func/date.i}


DEFINE VARIABLE lcBarrStatus AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcBarrComList AS CHARACTER NO-UNDO. 

def stream sout.
output stream sout to as_yts2179.log.

put stream sout unformatted "MSISDN|Y_DATA_ACTIVATED|CURRENT_STATUS" skip.
FOR EACH msrequest where
   msrequest.brand = "1" and
   msrequest.reqtype = 35 and
   msrequest.reqstatus  = 2 and
   msrequest.reqcparam1 = "Y_DATA" AND
   msrequest.reqiparam3 = 2 and
   msrequest.reqiparam4 = 2 and
   msrequest.actstamp > 20100301 NO-LOCK use-index reqtype:

   find MobSub where
        MobSub.msseq = msrequest.msseq NO-LOCK no-error.
   IF NOT AVAIL MobSub then do:
      put stream sout unformatted msrequest.cli "|" fts2hms(msrequest.actstamp) "|TERMINATED" skip.
      next.
   end.

   RUN checkmsbarring(
         INPUT MobSub.msseq,
         INPUT katun,
         OUTPUT lcBarrComList,
         OUTPUT lcBarrStatus).
 
   put stream sout unformatted MobSub.cli "|" fts2hms(msrequest.actstamp) "|" lcBarrStatus skip.
end.
