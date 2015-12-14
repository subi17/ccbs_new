/* Monthly script to add residual value fees to list of customers.
   Customer data must be in this order and delimited with ;
   MSISDN;SubscriptionID;Importe;ContractName;Valid To
   Change input and output filenames according to ticket.
   Before actual run simulation can be done with 
   llcreatefee false value */

{timestamp.i}

def stream sread.
def stream slog.

input stream sread from "/apps/yoigo/tms_support/billing/monthly_scripts/logs/yot_4187.csv".
output stream slog to "/apps/yoigo/tms_support/billing/monthly_scripts/logs/yot_4187.log".

/* Change here FALSE if only pre check done */
def var llcreatefee as log no-undo init true.

def var lcline as char no-undo.
def var limsseq as int no-undo.
def var liCustnum as int no-undo.
def var lccli as char no-undo.
def var ldamt as dec no-undo.
def var i as int no-undo.
def var j as int no-undo.
def var licheck as int no-undo.
def var limiss as int no-undo.
def var liexist as int no-undo.
def var lidouble as int no-undo.
def var lcbillcode as char no-undo.
def var lcmemo as char no-undo.
def var lccalcobj as char no-undo.
DEFINE VARIABLE liInvCust AS INTEGER NO-UNDO. 
def var liperiod as int no-undo.
def var lcPayTerm as char no-undo.
def var ldaPayTermTodate as date no-undo.

def temp-table ttsub no-undo
   field msseq as int
   index msseq msseq.

assign
   lcbillcode = "RVTERMF"
   lccalcobj  = "RVTERM".

if not can-find(first billitem where 
                      billitem.brand = "1" and
                      billitem.billcode = lcbillcode) then do:
   message "Unknown billing item"
   view-as alert-box.
   return.
end.
put stream slog unformatted lcLine "MSISDN;SubscriptionID;Importe;ContractName;Valid To;Status" skip.

repeat:

   import stream sread unformatted lcline.
   
   if lcline = "" OR lcLine begins "MSISDN" then next. 
   
   assign 
      limsseq = int(entry(2,lcline,";"))
      lccli = entry(1,lcline,";")
      ldAmt = dec(entry(3,lcline,";"))
      lcPayTerm = TRIM(entry(4,lcline,";"))
      ldaPayTermTodate = date(entry(5,lcline,";")) no-error.

   if error-status:error or limsseq = 0 then do:
      put stream slog unformatted lcLine ";" "ERROR:Invalid format" skip.
      next.
   end.  
   
   if can-find(first ttsub where ttsub.msseq = limsseq) then do:
       lidouble = lidouble + 1.
       put stream slog unformatted lcLine ";" "ERROR:Duplicate" skip.
       next.
   end.
   
   create ttsub.
   ttsub.msseq = limsseq.
   
   find first mobsub where
              mobsub.msseq = limsseq no-lock no-error.
   if not available mobsub then do:
      find first termmobsub where
                 termmobsub.msseq = limsseq no-lock no-error.
      if not avail termmobsub then do:
         put stream slog unformatted lcLine ";" "ERROR:MobSub not found" skip.
         next.
      end.
      liInvCust = termmobsub.invcust.
      if termmobsub.cli ne lccli then do:
         put stream slog unformatted lcLine ";" "ERROR:MobSub MSISDN is different" skip.
         licheck = licheck + 1.
         next.
      end.
   end.
   else do:
      liInvCust = mobsub.invcust.
      if mobsub.cli ne lccli then do:
         put stream slog unformatted lcLine ";" "ERROR:MobSub MSISDN is different" skip.
         licheck = licheck + 1.
         next.
      end.
   end.

   find first dccli where dccli.brand = "1" and 
                          dccli.msseq = limsseq and
                          dccli.dcevent = lcPayTerm and
                          dccli.validto = ldaPayTermTodate exclusive-lock no-error.
   if not avail dccli then do:
      put stream slog unformatted lcLine ";" "ERROR:Payterm contract not found" skip.
      licheck = licheck + 1.
      next.
   end.
      
   FIND fixedfee NO-LOCK where
        fixedfee.brand = "1" and
        fixedfee.custnum = liInvCust and
        fixedfee.hosttable = "mobsub" and
        fixedfee.billcode begins "payterm" and
        fixedfee.keyvalue = string(dccli.msseq) and
        fixedfee.sourcetable = "dccli" and
        fixedfee.sourcekey = string(dccli.PerContractID) no-ERROR.

   IF NOT AVAIL fixedfee THEN DO:
      put stream slog unformatted lcLine ";" "ERROR:Payterm fee not found "
      skip.
      licheck = licheck + 1.
      next.
   END.

   liperiod = YEAR(dccli.validto + 1) * 100 + MONTH(dccli.validto + 1).

   if can-find(first singlefee use-index hosttable where
        singlefee.brand = "1" and
        singlefee.hosttable = "MobSub" and
        singlefee.keyvalue = string(limsseq) and
        singlefee.billperiod  = liperiod and
        singlefee.calcobj = lccalcobj) 
   then do:
      put stream slog unformatted lcLine ";" "ERROR:Single Fee already exists" skip.
      liexist = liexist + 1.
      next.
   end.
      
   i = i + 1.
   
   if llcreatefee then do :
      CREATE SingleFee.
      ASSIGN
      SingleFee.Brand       = "1" 
      SingleFee.FMItemId    = NEXT-VALUE(bi-seq)
      SingleFee.CustNum     = liInvCust    
      SingleFee.BillTarget  = 1
      SingleFee.CalcObj     = lccalcobj 
      SingleFee.BillCode    = lcBillCode       
      SingleFee.BillPeriod  = liperiod
      SingleFee.Concerns[1] = (YEAR(dccli.validto + 1) * 10000 + 
                               MONTH(dccli.validto + 1) * 100  + day(dccli.validto + 1))
      SingleFee.Amt         = ldAmt
      SingleFee.Memo[1]     = ""
      SingleFee.Memo[2]     = ""
      SingleFee.HostTable   = "MobSub"
      SingleFee.KeyValue    = STRING(limsseq)
      SingleFee.BillType    = "SF"
      SingleFee.Contract    = ""
      SingleFee.Active      = TRUE
      SingleFee.FeeModel    = ""
      SingleFee.VATIncl     = FALSE
      SingleFee.OrderID     = FixedFee.OrderID
      SingleFee.SourceTable = "DCCLI"
      SingleFee.SourceKey   = STRING(DCCLI.PerContractID)
      j = j + 1.

      dccli.amount = ldamt.
      
   end.

   put stream slog unformatted lcLine ";" "done" skip.
   
   pause 0.
   disp i j licheck limiss liexist lidouble with 1 down.
end.

disp i j licheck limiss liexist lidouble.

input stream sread close.
output stream slog close.
