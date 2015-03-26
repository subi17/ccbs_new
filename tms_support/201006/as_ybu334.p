def stream slog.
output stream slog to as_ybu334.log.
def buffer bmservicelimit for mservicelimit.

DEFINE VARIABLE ldaDate AS DATE NO-UNDO. 
DEFINE VARIABLE ldaDateEnd AS DATE NO-UNDO. 
DEFINE VARIABLE litime AS INTEGER NO-UNDO. 

/* Customer number, MSISDN, billing item, number of CDRs, euro amount, invoice number (if invoiced) */

{date.i}

DEFINE VARIABLE ldeAmount AS DECIMAL NO-UNDO. 
DEFINE VARIABLE liAmount AS INTEGER NO-UNDO. 
DEFINE VARIABLE liInvoice AS INT NO-UNDO.
DEFINE VARIABLE llMdub AS LOGICAL NO-UNDO. 
DEFINE VARIABLE i AS INTEGER NO-UNDO. 
DEFINE VARIABLE lcBillCode AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcExtInvoice AS CHARACTER NO-UNDO. 

FOR EACH mservicelimit where 
   slseq = 11 and
   fromts = endts NO-LOCK:

   find first bmservicelimit where
      bmservicelimit.MsSeq = mservicelimit.msseq and
      bmservicelimit.slseq = 12 and
      bmservicelimit.fromts = mservicelimit.fromts + 0.00001 NO-LOCK no-error.
   
   IF NOT AVAIL bmservicelimit then do:
      i = i + 1.
      disp i.
      pause 0.

      find msowner where
           msowner.msseq = mservicelimit.msseq and
           msowner.tsend >= mservicelimit.fromts and
           msowner.tsbegin <= mservicelimit.endts NO-LOCK.

      find mobsub where
           mobsub.msseq = msowner.msseq NO-LOCK no-error.
      
      fSplitTS(mservicelimit.fromts + 0.00001, output ldaDate, output litime).
      ldaDateEnd = DATE(MONTH(ldaDate) + 1, 1, 
         (IF MONTH(ldaDate) eq 12 then YEAR(ldaDate) + 1 ELSE YEAR(ldaDate))).

      ldeAmount = 0.
      liAmount = 0.
      liInvoice = 0.
      llMdub = false.
      lcBillCode = "".
      
      FOR EACH mobcdr NO-LOCK where
               mobcdr.cli = msowner.cli and
               mobcdr.datest >= ldaDate and 
               mobcdr.datest < ldaDateEnd and 
               mobcdr.ccn = 93 use-index CLI:

         if errorcode ne 0 then next.
         
         if mobcdr.datest = ldaDate AND mobcdr.timestart < liTime
            THEN NEXT.

         if billcode = "mdub" then do:
            if lcBillCode = "14100001" then disp "wtf".
            if amount ne 0 then disp amount.
            llMdub = true.
            lcBillCode = mobcdr.billcode.
         end.
         else if mobcdr.billcode eq "14100001" then do:
            if lcBillCode = "mdub" then disp "wtf".
            lcBillCode = mobcdr.billcode.
         end.
         else
           disp mobcdr.billcode msseq mobcdr.datest.
         
         ldeAmount = ldeAmount + mobcdr.amount.
         liAmount = liAmount + 1.
         if mobcdr.invseq > 0 and liInvoice eq 0 then do:
            FIND FIRST InvSeq NO-LOCK WHERE
                InvSeq.InvSeq = mobcdr.invSeq NO-ERROR.
            if invseq.billed then do:
               
               find invoice where
                    invoice.invnum = invseq.invnum NO-LOCK.
               liInvoice = InvSeq.InvNum. 
               lcExtInvoice = Invoice.ExtInvId.
            end.
         end.
         if liInvoice > 0 and mobcdr.invseq = 0 then disp "wtf".

      end. 

      put stream slog unformatted 
         msowner.custnum "|" msowner.cli "|" lcBillCode "|" liAmount "|"
         ldeAmount "|" liInvoice "|" lcExtInvoice skip.

   end.
end.
