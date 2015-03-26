def var i     as int no-undo.
def var j     as int no-undo.
def var k     as int no-undo.
def var ldamt as dec no-undo.

for each invoice no-lock where      
         invoice.brand = "1" and
         invoice.custnum = 10292 and
         invoice.invdate >= 11/1/7 and
         invoice.invdate <= 11/30/7 and
         invoice.invtype = 1,
   first invrow of invoice no-lock where
         invrow.rowtype = 5,
    each invseq no-lock where
         invseq.msseq  = invoice.msseq and
         invseq.billed = false and
         invseq.fromdate < 11/1/7:
         
   
   i = i + 1.

   ldamt = 0.
   for each mobcdr no-lock where
            mobcdr.invcust = invseq.custnum and
            mobcdr.invseq  = invseq.invseq  and
            mobcdr.datest < 11/1/7:
      ldamt = ldamt + mobcdr.amount.
   end.
           
   disp ldamt.         

   /* if mincons already billed and cdr amount more than that -> 
      credit the mincons */
      
   if ldamt >= 6 then do:
   
      if can-find(first singlefee no-lock use-index hosttable where
                 singlefee.hosttable = "mobsub" and
                 singlefee.keyvalue  = string(invoice.msseq) and
                 singlefee.calcobj   = "CredMC11")
      then next. 
      
      j = j + 1.

      CREATE SingleFee.

      ASSIGN
      SingleFee.Brand       = "1" 
      SingleFee.FMItemId    = NEXT-VALUE(bi-seq)
      SingleFee.CustNum     = InvSeq.Custnum    
      SingleFee.BillTarget  = 1
      SingleFee.CalcObj     = "CredMC11"
      SingleFee.BillCode    = InvRow.BillCode       
      SingleFee.BillPeriod  = 200711
      SingleFee.Concerns[1] = 20071119
      SingleFee.Amt         = -1 * invrow.amt
      SingleFee.Memo[1]     = ""
      SingleFee.Memo[2]     = ""
      SingleFee.HostTable   = "MobSub"
      SingleFee.KeyValue    = STRING(Invoice.MsSeq)
      SingleFee.BillType    = "SF"
      SingleFee.Contract    = ""
      SingleFee.Active      = TRUE
      SingleFee.FeeModel    = ""
      SingleFee.VATIncl     = Invoice.VatIncl.

   end.

   else if ldamt > 0 then do:
      message "check:" 
         invseq.custnum invseq.msseq invseq.fromdate invseq.todate
      view-as alert-box.
    
   end.

   /*
   pause 0.
   disp i j k with 1 down.
   */
   
end.
         
disp i j k.
         
         
         
