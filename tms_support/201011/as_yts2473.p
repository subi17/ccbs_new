{Syst/commpaa.i}
katun = "Qvantel".
gcBrand = "1".

{Func/timestamp.i}
{Func/fmakemsreq.i}

DEFINE VARIABLE lcLine AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcCommLine AS CHARACTER NO-UNDO. 
DEFINE VARIABLE ldeActStamp AS DECIMAL NO-UNDO.

input from as_yts2473_december.input.
def stream slog.
output stream slog to as_yts2473_december.log append.

DEFINE VARIABLE i AS INTEGER NO-UNDO. 
DEFINE VARIABLE lcCli AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcICC AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcIMSI AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcInfo AS CHARACTER NO-UNDO. 
def var lireq as int no-undo.

DEFINE VARIABLE lcCliType AS CHARACTER NO-UNDO. 
DEFINE VARIABLE j AS INTEGER NO-UNDO. 
looppi:
repeat:
   import unformatted lcLine.

   j = j + 1.
 
   if j <= 500 then next.  
/*   if j > 500 then leave looppi.  */

   find first mobsub where
      mobsub.msseq = int(lcLine) NO-LOCK no-error.

   IF NOT AVAIL mobsub then do:
      put stream slog lcLine "||ERROR:NOT FOUND" skip.
      next.
   end.
   
   if lookup(mobsub.clitype,"contrd1,contrd2,tarjrd1") = 0 then do:
      put stream slog unformatted mobsub.cli "|" mobsub.msseq "|" mobsub.clitype "|SKIPPED" skip.
      next.
   end.

   lcCliType =  "".
   ldeActstamp = 0.
   
   for each msrequest NO-LOCK where
      msrequest.msseq = mobsub.msseq and
      msrequest.reqtype = 0 and
      msrequest.reqstatus = 2 
      by msrequest.actstamp desc:
      lcCliType  = msrequest.reqcparam2.
      ldeActstamp = msrequest.actstamp.
      leave.
   end.
   if lcCliType ne mobsub.clitype then do:
      MESSAGE mobsub.msseq "faa" VIEW-AS ALERT-BOX.
      next looppi.
   end.
   
   i = 0.
   FOR EACH msrequest where
            msrequest.msseq = mobsub.msseq and
            msrequest.reqtype = 1 and
            msrequest.reqstatus = 2 and
            msrequest.reqcparam1 = "SHAPER" and
            msrequest.actstamp >= ldeActstamp NO-LOCK:
      
      if index(msrequest.reqcparam2,mobsub.clitype) > 0 then do:
         
         put stream slog unformatted mobsub.cli "|" mobsub.msseq "|" mobsub.clitype "|OK " msrequest.reqcparam1 "=" msrequest.reqcparam2 skip.
         next looppi.
      end.
      else if msrequest.reqcparam2 ne "" then dO:
         MESSAGE msrequest.msseq msrequest.reqcparam2 VIEW-AS ALERT-BOX.
         next looppi.
      end.
   end.  
   
   do trans:
   
   /* create change request */
   liReq = fServiceRequest(MobSub.msseq,
                           "SHAPER",
                           1,
                           MobSub.clitype,
                           fmakets(),
                           "", /* salesman */
                           FALSE,      /* fees */
                           FALSE,      /* sms */
                           "YTS-2472",
                           OUTPUT lcInfo). 

   put stream slog unformatted mobsub.cli "|" mobsub.msseq "|" mobsub.clitype "|" fts2hms(ldeActstamp)  
     "|FIXED " liReq " : " lcInfo skip.
   end.

end.


input close.
/*output stream slog_create close.*/
output stream slog close.
