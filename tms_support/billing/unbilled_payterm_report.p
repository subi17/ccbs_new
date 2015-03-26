/* ----------------------------------------------------------------------
  module .......: unbilled_payterm_report.p
  task .........: 
  application ..: tms
  author .......: vikas
  created ......: 19.04.11
  version ......: yoigo
---------------------------------------------------------------------- */

define variable ldeamount   as decimal   no-undo.

output to "/apps/yoigo/tms_support/billing/unbilled_payterm_report.csv".

put unformatted "custnum" ";"
                "subs id" ";"
                "msisdn"  ";"
                "billingitem" ";"
                "amount" skip.

for each daycampaign where
         daycampaign.brand   = "1" and
         daycampaign.dcevent begins "payterm18" no-lock,
    each fixedfee no-lock where
         fixedfee.brand = "1" and
         fixedfee.hosttable = "mobsub" and
         fixedfee.feemodel  = replace(daycampaign.dcevent,"18_",""),
    first msowner where msowner.msseq = int(fixedfee.keyvalue) no-lock:

   ldeamount = 0.

   for each ffitem of fixedfee where
            ffitem.billed = false and
            ffitem.billper < year(today) * 100 + month(today) no-lock:
       ldeamount = ldeamount + ffitem.amt.
   end. /* for each ffitem of fixedfee where */

   if ldeamount > 0 then
   put unformatted string(msowner.custnum) ";"
                   string(msowner.msseq)   ";"
                   msowner.cli             ";"
                   fixedfee.billcode       ";"
                   string(ldeamount)       skip.
end. /* for each daycampaign where */

output close.