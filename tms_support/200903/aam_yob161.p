/* based on 200902/as_yob150.p */

{Syst/commpaa.i}
katun = "ari".
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
def var j as int no-undo.

def stream slog.
def stream sread.

input stream sread from /apps/snet/200903/Billing_permission_200904.txt.
output stream slog to /apps/snet/200903/aam_yob161.log append.


repeat:
   import stream sread unformatted lcLine.

   i = i + 1.

   assign
   liMsSeq = int(trim(entry(1,lcLine,chr(9))))
   lcOrgId  = trim(entry(3,lcLine,chr(9)))
   lcDate = trim(entry(4,lcLine,chr(9)))
   liStatus = int(trim(entry(5,lcLine,chr(9))))
   no-error.
   
   if error-status:error then next.
   
   ldaDate = date(
               INT(SUBSTRING(lcDate,5,2)),
               INT(SUBSTRING(lcDate,7,2)),
               INT(SUBSTRING(lcDate,1,4))).

   find mobsub where mobsub.msseq = liMsSeq NO-LOCK NO-ERROR.
   
   IF NOT AVAIL mobsub then do:
      find termmobsub WHERE termmobsub.msseq = liMsSeq NO-LOCK NO-ERROR.
      IF NOT AVAIL termmobsub then do:
         put stream slog unformatted
            lcLine  chr(9) liCustnum chr(9) "msseq NOT FOUND" skip.
         next.
      END.
      liCustnum = termmobsub.invcust.
   END.
   ELSE liCustnum = mobsub.invcust.
      
   find customer where 
      customer.brand = "1" and
      customer.custnum = liCustnum NO-LOCK.

   fGetLimit(licustnum, limsseq, 3, 0, 0, ldaDate).
   
   if avail limit then do:
      if limit.limitamt = listatus then do:
         put stream slog unformatted lcLine chr(9) liCustnum 
                chr(9) "ALREADY SAME VALUE" skip.
         next.
      end.
      if limit.fromdate >= ldaDate then do:
         put stream slog unformatted 
            lcLine chr(9) liCustnum chr(9) "UPDATED ALREADY TO VALUE "
            Limit.LimitAmt skip.
         next.
      end.
   end.
  
  
   j = j + 1.

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
   
   put stream slog unformatted 
      lcLine chr(9) liCustnum chr(9) "OK".
       
   if lcOrgId ne "" and customer.orgid ne lcOrgId then 
   put stream slog unformatted 
       chr(9) "NOTE: " lcOrgId 
       " is not current subscription's customer ID".
  
   put stream slog skip.

   pause 0.
   disp i j with 1 down.
   
end.

input stream sread close.
output stream slog close.

