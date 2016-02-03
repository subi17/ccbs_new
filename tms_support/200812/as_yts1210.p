{Syst/commpaa.i}
katun = "anttis".
gcBrand = "1".
{Func/timestamp.i}
{Func/barrfunc.i}

DEF VAR lrOLBRec      AS RECID                  NO-UNDO.

def buffer MsRequest2 for MsRequest.
def stream slog.
DEFINE VARIABLE lcError AS CHARACTER NO-UNDO.
output stream slog to /apps/snet/200812/as_yts1210.log.

FOR EACH msrequest where
   msrequest.brand = "1" and
   msrequest.reqtype = 0 and
   msrequest.reqcparam1 = "contrd1" and
   msrequest.reqstatus = 2 NO-LOCK:

   FIND mobsub where
      mobsub.msseq  = msrequest.msseq NO-LOCK NO-ERROR.

   if avail mobsub and
      mobsub.activationdate < 9/12/2008 then do:
  
      if lookup(mobsub.cli,"622267513,622244698") > 0 then next.
    
      lrOLBRec =  fCheckRestore(mobsub.msseq, "olb").

      if lrOLBRec NE ? THEN DO:
          
         FIND MsRequest2 WHERE RECID(MsRequest2) = lrOLBRec 
                     NO-LOCK NO-ERROR.

         IF AVAILABLE MsRequest2 THEN DO:
         

          RUN barrengine.p (MobSub.MsSeq,
                      "UN" + "Y_PRODINT",
                      "5",                 /* source  */
                      "",                  /* creator */
                      fMakeTS(),           /* activate */
                      "",                  /* sms */
                      OUTPUT lcError).

         put stream slog unformatted 
            mobsub.cli " " 
            msrequest.actstamp " " 
            mobsub.clitype " "
            mobsub.msstatus " "
            msrequest2.reqcparam1 " "
            msrequest2.reqstatus " "
            lcError skip.
 
         end.

      end.            

   end.
   
END.

output stream slog close.
