{commpaa.i}
{fmakemsreq.i}
katun = "CreSub".
gcBrand = "1".
DEFINE VARIABLE liRequest AS INTEGER NO-UNDO.
DEFINE VARIABLE lcResult AS CHARACTER NO-UNDO. 

DEFINE VARIABLE i AS INTEGER NO-UNDO. 
output to /home/anttis/preterm.txt append.
looppi:

FOR EACH mobsub where
   mobsub.brand = "1" and
   mobsub.paytype = true and
   mobsub.clitype ne "tarj3" NO-LOCK:
   
   FOR EACH msrequest where
      msrequest.brand  = "1" and
      msrequest.reqtype = 0 and
      msrequest.cli = mobsub.cli and 
      msrequest.reqcparam1 begins "cont" NO-LOCK.
      next looppi.
   end.
  
   FIND FIRST dccli where  
      dccli.msseq = mobsub.msseq and
      dccli.dcevent = "PRETERM" NO-LOCK NO-ERROR.

   IF AVAIL dccli then do:
      next looppi.
   end.

   FIND FIRST order where
      order.msseq = mobsub.msseq NO-LOCK NO-ERROR.
  
   FIND FIRST orderaccessory of order NO-LOCK NO-ERROR.
   IF AVAIL orderaccessory then do: 
      
      FIND FIRST msrequest where
         msrequest.brand  = "1" and
         msrequest.reqtype = 13 and
         msrequest.cli = mobsub.cli and
         msrequest.msseq = mobsub.msseq NO-LOCK NO-ERROR. 

      IF NOT AVAIL msrequest then do:
         FIND FIRST msrequest where
            msrequest.brand  = "1" and
            msrequest.reqtype = 13 and
            msrequest.msseq = mobsub.msseq NO-LOCK NO-ERROR. 
         IF NOT AVAIL msrequest then next looppi.
      end.
      
      liRequest = fPCActionRequest(mobsub.msseq,
                                   "PRETERM",
                                   "act",
                                   order.crstamp,
                                   FALSE,
                                   "1",
                                   (if avail msrequest then 
                                   "OrigReq:" + STRING(msrequest.msrequest)
                                    else ""),
                                   OUTPUT lcResult).

      put unformatted mobsub.cli " " lcResult skip.
   end.
END.
output close.
