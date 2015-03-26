DEFINE VARIABLE lcKey AS CHARACTER NO-UNDO. 
def stream sout.
output stream sout to /apps/yoigo/tms_support/billing/check_incorrect_payterm_fees.log.
put stream sout unformatted 
   "FeeModel|Amount|Custnum|TargetType|Target|FFNum|Unbilled_PAYTERMEND|Reason" skip.
FOR EACH fixedfee where
         fixedfee.brand = "1" and
         fixedfee.billcode = "PAYTERM18"
        NO-LOCK:
   if 
   
      (CalcObj = "PAYTERM18_15" and
      amt ne 15) or
      
      (CalcObj = "PAYTERM18_20" and
      amt ne 20) or
      
      (CalcObj = "PAYTERM18_10" and
      amt ne 10) then do:

      lckey =  "1" + chr(255) + string(fixedfee.custnum).
      
      find first eventlog where
                eventlog.tablename = "fixedfee" and
                eventlog.key begins lckey and
                eventlog.action = "modify" and
                index(modifiedfields,"amt") > 0 
      NO-LOCK no-error.

      find first singlefee where
                 singlefee.brand = "1" and
                 singlefee.custnum = fixedfee.custnum and
                 singlefee.hosttable = "mobsub" and
                 singlefee.keyvalue = fixedfee.keyvalue and
                 singlefee.billcode = "PAYTERMEND" and
                 singlefee.billed = false NO-LOCK no-error.

      IF AVAIL eventlog then
      put stream sout unformatted
         fixedfee.CalcObj "|"
         fixedfee.amt "|"
         fixedfee.custnum "|"
         fixedfee.hosttable "|"
         fixedfee.keyvalue "|"
         fixedfee.ffnum "|"
         avail(singlefee) "|"
         "user:" eventlog.usercode
         ",time:" eventlog.eventtime
         ",fields:" eventlog.modifiedfields      
        ",values:" eventlog.datavalues skip.
      else put stream sout unformatted
         fixedfee.CalcObj "|"
         fixedfee.amt "|"
         fixedfee.custnum "|"
         fixedfee.hosttable "|"
         fixedfee.keyvalue "|"
         fixedfee.ffnum "|"
         avail(singlefee) "|"
         "user:?"
         ",time:?"
         ",fields?:"      
        ",values:?" skip.

      end.
end.
