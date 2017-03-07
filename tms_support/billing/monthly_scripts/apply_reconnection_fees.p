{Func/timestamp.i}

def stream sread.
def stream slog.

input stream sread from "/apps/yoigo/tms_support/billing/monthly_scripts/logs/yot_3759.csv".

output stream slog to "/apps/yoigo/tms_support/billing/monthly_scripts/logs/yot_3759.log".

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

def temp-table ttsub no-undo
   field msseq as int
   index msseq msseq.
   
lcmemo = "Cargo de Restablecimiento de servicio. Según clausula 6 de condiciones generales de contrato. Yoigo ha cargado 10 euros en el concepto de restablecimiento de servicio para sufragar los gastos que genera un cliente que devuelve su factura por banco (comisión del banco por rechazo, coste de SMS de recobro, Coste de atencion al cliente y cobros, comisiones de agencias recobro, comisiones de tarjeta de crédito y otros gastos de gestión) Este cargo se aplicara cada vez que el cliente impaga su factura y entra en circuito de recobro y se le desconecta el servicio.".

assign
   lcbillcode = "RECONNECTION"
   lccalcobj  = "YOT-3748_RF02".

if not can-find(first billitem where 
                    billitem.brand = "1" and
                    billitem.billcode = lcbillcode) then do:
   message "Unknown billing item"
   view-as alert-box.
   return.
end.

ldamt = 10.

message "Fees will " trim(string(llcreatefee,"/NOT")) " be created " +
        string(ldamt) view-as alert-box.

import stream sread unformatted lcline.

repeat:

   import stream sread unformatted lcline.
   
   if lcline = "" OR lcLine begins "Customer" then next. 
   
   assign 
      licustnum = int(entry(1,lcLine,";"))
      limsseq = int(entry(2,lcline,";"))
      lccli = entry(3,lcline,";") no-error.

   if error-status:error or limsseq = 0 then next.
   
   if can-find(first ttsub where ttsub.msseq = limsseq) then do:
       lidouble = lidouble + 1.
       put stream slog unformatted lcLine ";" "Duplicate" skip.
       next.
   end.
   
   create ttsub.
   ttsub.msseq = limsseq.
   
   find first mobsub where
      mobsub.msseq = limsseq no-lock no-error.
   if not available mobsub then do:
      find termmobsub NO-LOCK where
           termmobsub.msseq = limsseq no-error.
      IF NOT AVAIL termmobsub then do:
         put stream slog unformatted lcLine ";" "TermMobSub not found" skip.
         next.
      end.
   
      if termmobsub.cli ne lccli then do:
         put stream slog unformatted lcLine ";" "TermMobSub MSISDN is different" skip.
         licheck = licheck + 1.
         next.
      end.

      liInvCust = termmobsub.invcust.
      
      if liInvCust ne liCustnum then do:
         put stream slog unformatted lcLine ";" "TermMobsub custnum is different" skip.
         licheck = licheck + 1.
         next.
      end.
      limiss = limiss + 1.
   end.
   else do:
      liInvCust = mobsub.invcust.
      if  mobsub.cli ne lccli then do:
         put stream slog unformatted lcLine ";" "MobSub MSISDN is different" skip.
         licheck = licheck + 1.
         next.
      end.
      if liInvCust ne liCustnum then do:
         put stream slog unformatted lcLine ";" "MobSub custnum is different" skip.
         licheck = licheck + 1.
         next.
      end.
   end.

   
   if can-find(first singlefee use-index hosttable where
        singlefee.brand = "1" and
        singlefee.hosttable = "MobSub" and
        singlefee.keyvalue = string(limsseq) and
        singlefee.billcode = lcbillcode and
        singlefee.amt = ldamt and
        singlefee.billperiod  = 201502 and
        singlefee.calcobj = lccalcobj) 
   then do:
      put stream slog unformatted lcLine ";" "Single Fee already exists" skip.
      liexist = liexist + 1.
      next.
   end.
       
   i = i + 1.
   
   if llcreatefee then do trans:
      j = j + 1.
      CREATE SingleFee.
      ASSIGN
      SingleFee.Brand       = "1" 
      SingleFee.FMItemId    = NEXT-VALUE(bi-seq)
      SingleFee.CustNum     = liInvCust    
      SingleFee.BillTarget  = 1
      SingleFee.CalcObj     = lccalcobj 
      SingleFee.BillCode    = lcBillCode       
      SingleFee.BillPeriod  = 201502
      SingleFee.Concerns[1] = 20150228
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
      j = j + 1.
      
      if lcmemo > "" then do:
         CREATE Memo.
         ASSIGN
         Memo.CreStamp  = fMakeTS()
         Memo.MemoSeq   = NEXT-VALUE(MemoSeq)
         Memo.Brand     = "1"
         Memo.MemoTitle = "Reconnection Fee"
         Memo.CreUser   = "Qvantel"
         Memo.HostTable = "SingleFee"
         Memo.KeyValue  = STRING(SingleFee.FMItemID)
         Memo.CustNum   = SingleFee.CustNum
         Memo.Memotext  = lcmemo.
      end.   
   end.

   put stream slog unformatted lcLine ";" "Single Fee created" skip.
   
   pause 0.
   disp i j licheck limiss liexist lidouble with 1 down.
end.

disp i j licheck limiss liexist lidouble.

input stream sread close.
output stream slog close.


