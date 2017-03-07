{Syst/commpaa.i}
katun = "anttis".
gcBrand = "1".
{Syst/tmsconst.i}
{Func/flimitreq.i}

DEFINE VARIABLE lcLine AS CHARACTER NO-UNDO. 
DEFINE VARIABLE liMsSeq AS INTEGER NO-UNDO. 
DEFINE VARIABLE liCustnum AS INTEGER NO-UNDO. 
DEFINE VARIABLE lcOrgId AS CHARACTER NO-UNDO. 
DEFINE VARIABLE liStatus AS INTEGER NO-UNDO. 
DEFINE VARIABLE lcDate AS CHARACTER NO-UNDO. 
DEFINE VARIABLE ldaDate AS DATE NO-UNDO.
DEFINE VARIABLE i AS INTEGER NO-UNDO. 

def stream slog.
input from /apps/snet/200902/Billing_permission_200903_-_Quantel.csv.
output stream slog to /apps/snet/200902/as_yob150.log append.
/*output stream slog to /apps/snet/200902/as_yob150.log.test. */

import unformatted lcLine.

repeat:
   import unformatted lcLine.

   i = i + 1.
   if i <= 5 then next.

   liCustnum = 0.
   liMsSeq = 0.
   
   liMsSeq = int(trim(entry(1,lcLine,";"))).
   lcOrgId  = trim(entry(3,lcLine,";")).
   lcDate = trim(entry(4,lcLine,";")).
   liStatus = int(trim(entry(5,lcLine,";"))).
   
   ldaDate = date(
               INT(SUBSTRING(lcDate,5,2)),
               INT(SUBSTRING(lcDate,7,2)),
               INT(SUBSTRING(lcDate,1,4))).

   find mobsub where mobsub.msseq = liMsSeq NO-LOCK NO-ERROR.
   
   IF NOT AVAIL mobsub then do:
      find termmobsub WHERE termmobsub.msseq = liMsSeq NO-LOCK NO-ERROR.
      IF NOT AVAIL termmobsub then do:
         disp lcLine  ";" liCustnum ";NOK msseq NOT FOUND".
         next.
      END.
      liCustnum = termmobsub.custnum.
   END.
   ELSE liCustnum = mobsub.custnum.
      
   find customer where 
      customer.brand = "1" and
      customer.custnum = liCustnum NO-LOCK.
/*   
   IF lcOrgId NE "" THEN DO:
      
      find customer where 
         customer.brand = "1" and
         customer.orgid = lcOrgId NO-LOCK NO-ERROR.
      
      IF NOT AVAIL customer then do:
         
         find customer where 
            customer.brand = "1" and
            customer.custnum = licustnum NO-LOCK NO-ERROR.
         
         IF customer.orgid ne lcOrgId then do:
            put stream slog unformatted lcLine ";" liCustnum ";NOK OrgId NOT FOUND" skip.
            next.
         END.
      
      END.
      ELSE licustnum = customer.custnum.

   END. 
  */  
   fGetLimit(licustnum, limsseq, 3, 0, 0, TODAY).
   
   if avail limit then do:
      if limit.limitamt = listatus then do:
         put stream slog unformatted lcLine ";" liCustnum ";OK ALLREADY" skip.
         next.
      end.
      if limit.fromdate >= ldaDate then do:
         put stream slog unformatted lcLine ";" liCustnum ";NOK: IS UPDATED ALLREADY TO VALUE " Limit.LimitAmt skip.
         next.
      end.
   end.
  
  
   fCreateLimitHistory(
      licustnum,
      limsseq,
      {&LIMIT_TYPE_BILLPERM},
      liStatus,
      0,
      0,
      FALSE,
      ldaDate,
      12/31/2049).
   
   
   if lcOrgId ne "" and customer.orgid ne lcOrgId then put stream slog unformatted lcLine ";" liCustnum ";OK NOTE: " lcOrgId " is not current subscription's customer ID " skip.
   else put stream slog unformatted lcLine  ";" liCustnum ";OK " skip.

end.

input close.
output stream slog close.
