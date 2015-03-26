{commpaa.i}
katun = "anttis".
gcBrand = "1".
{tmsconst.i}
{flimitreq.i}

DEFINE VARIABLE lcLine AS CHARACTER NO-UNDO. 
DEFINE VARIABLE liMsSeq AS INTEGER NO-UNDO. 
DEFINE VARIABLE liCustnum AS INTEGER NO-UNDO. 
DEFINE VARIABLE liStatus AS INTEGER NO-UNDO. 
def stream slog.
input from /apps/snet/200902/DenyListCustomerCleanUP20090127.txt.
output stream slog to /apps/snet/200901/as_ysu8_4.log.
DEFINE VARIABLE i AS INTEGER NO-UNDO. 
import unformatted lcLine.
repeat:
   import unformatted lcLine.
   liMsSeq = int(trim(entry(2,lcLine,"|"))).
   liCustnum = int(trim(entry(3,lcLine,"|"))).
   liStatus = int(trim(entry(5,lcLine,"|"))).
   /*
   i = i + 1.
   if i <= 2 then next.
   */
   find mobsub where mobsub.msseq = liMsSeq NO-LOCK NO-ERROR.
   
   find customer where customer.custnum = liCustnum NO-LOCK NO-ERROR.
   if not avail customer then do:
      MESSAGE liCustnum VIEW-AS ALERT-BOX.
      next.
   end.

   IF AVAIL mobsub THEN DO:
     /*
     fGetLimit(licustnum, mobsub.msseq, 3, 0, 0, TODAY).
     if limit.limitamt = 0 then next.
      */
      fCreateLimitHistory(
         licustnum,
         mobsub.msseq,
         {&LIMIT_TYPE_BILLPERM},
         liStatus,
         0,
         TODAY,
         12/31/2049).
         
      put stream slog unformatted mobsub.msseq "|OK " listatus skip.
   END.
   ELSE DO:
      find termmobsub WHERE termmobsub.msseq = liMsSeq NO-LOCK NO-ERROR.
     /*
     fGetLimit(licustnum, termmobsub.msseq, 3, 0, 0, TODAY).
     if limit.limitamt = 0 then next.
      */
      fCreateLimitHistory(
         licustnum,
         termmobsub.msseq,
         {&LIMIT_TYPE_BILLPERM},
         liStatus,
         0,
         TODAY,
         12/31/2049). 
         
      put stream slog unformatted termmobsub.msseq "|OK TERMINATED " listatus skip.
   END.
end.

input close.
output stream slog close.
