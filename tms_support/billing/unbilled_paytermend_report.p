/* ----------------------------------------------------------------------
  module .......: unbilled_paytermend_report.p
  task .........: 
  application ..: tms
  author .......: vikas
  created ......: 19.04.11
  version ......: yoigo
---------------------------------------------------------------------- */

{commpaa.i}
katun = "anttis".
gcBrand = "1".
{flimitreq.i}
{tmsconst.i}


def stream sout.
output stream sout to "/apps/yoigo/tms_support/billing/unbilled_penalty_fees_201202.txt".

put stream sout unformatted "custnum" ";"
                "subs id" ";"
                "msisdn"  ";"
                "billingitem" ";"
                "amount" skip.


DEFINE VARIABLE i AS INTEGER NO-UNDO. 
DEFINE VARIABLE ldetotal AS DECIMAL NO-UNDO. 

run pReport("paytermend").
run pReport("termperiod").
         
disp i ldeTotal.

output stream sout close.

DEFINE TEMP-TABLE ttCustnum
FIELD custnum like customer.custnum
INDEX custnum IS PRIMARY UNIQUE custnum.  

PROCEDURE pReport:

   DEFINE INPUT PARAMETER icBillcode AS CHARACTER NO-UNDO. 

   for each singlefee where
            singlefee.brand     = "1" and
            singlefee.hosttable = "mobsub" and
            singlefee.billcode  = icBillcode and  
            singlefee.billed    = false and
            singlefee.billperiod = 201202 NO-LOCK:

     find first msowner where msowner.msseq = int(singlefee.keyvalue) and
           msowner.tsbegin < 20120301 use-index msseq NO-LOCK.
   
      fGetLimit(single.custnum, int(singlefee.keyvalue), {&LIMIT_TYPE_BILLPERM}, 0, 0, TODAY).

      if avail(limit) and limit.limitamt > 0 then next.
      
      FIND FIRST ttCustnum where
                 ttCustnum.custnum = singlefee.custnum NO-LOCK no-error.
      IF NOT AVAIL ttCustnum then do:
         create ttCustnum.
         assign
            ttCustnum.custnum = singlefee.custnum.
      end.

      i = i + 1.
      ldeTotal = ldeTotal + singlefee.amt.
      disp i ldeTotal.
      pause 0.

      put stream sout unformatted
                      string(msowner.custnum) ";"
                      string(msowner.msseq)   ";"
                      msowner.cli             ";"
                      singlefee.billcode      ";"
                      string(round(singlefee.amt,2))   skip.
   end. /* for each singlefee where */


END PROCEDURE. 

def stream sinv.
output stream sinv to "/apps/yoigo/tms_support/billing/invoices_to_be_deleted.txt".
put stream sinv unformatted "invnum|custnum|inv_amt" skip.

FOR EACH ttCustnum NO-LOCK:
   FIND FIRST invoice where
         invoice.brand = "1" and
         invoice.custnum = ttCustnum.Custnum and
         invoice.invtype  = 1 and
         invoice.invdate = 3/1/2012 NO-LOCK no-error.
   IF NOT AVAIL invoice then next.

    put stream sinv unformatted invoice.invnum "|" invoice.custnum "|" invoice.invamt skip.
end.
