def var i       as int no-undo. 
def var j       as int no-undo.
def var ldamt   as dec no-undo.
def var ldfirst as dec no-undo.
def var ldlast  as dec no-undo.
def var ldmin   as dec no-undo init 6.
def var liday   as int no-undo.
def var ldshare as dec no-undo.
def var ldtot   as dec no-undo.
def var ldfat   as dec no-undo.
def var lctype  as char no-undo.

for each invoice no-lock where
         brand   = "1"    and
         invdate = 9/13/7 and
         invtype = 1      and
         amtexclvat < 6:

   i = i + 1.
   pause 0.
   disp i j with 1 down.
            
   if can-find(first invrow of invoice where invrow.rowtype = 5)
   then next.

   ldfat = 0.
   for each invrow of invoice where invrow.rowtype = 7 no-lock:
      ldfat = ldfat - invrow.amt.
   end.
   if ldfat + invoice.amtexclvat > 6 then next.
    
   ldfirst = 99999999.
   ldlast  = 0.
   lctype = "".
   
   for each msowner no-lock where
            msowner.msseq = invoice.msseq 
   break by msowner.tsend desc:
      if first-of(msowner.tsend) then lctype = msowner.clitype.
      
      ldfirst = min(ldfirst,msowner.tsbeg).
      ldlast  = max(ldlast,msowner.tsend).
   end.
   
   if lctype ne "cont" then next.
   
   if ldfirst > 20070801 then next.
    
   ldamt = ldmin.   

   if ldlast > 20070801 and ldlast < 20070901 then do:
      liday = integer(substring(string(ldlast),7,2)).
      
      if liday > 1 then ldshare = (liday - 1) / 31.
      
      else ldshare = 0.
      
      ldamt = round(ldamt * ldshare,2).
   
   end.
   else if ldlast < 20070801 then next.
   
   
   ldamt = max(0,ldamt - (invoice.amtexclvat + ldfat)).
      
   if ldamt = 0 then next.
   
   j = j + 1.

   CREATE SingleFee.

   ASSIGN
      SingleFee.Brand       = "1" 
      SingleFee.FMItemId    = NEXT-VALUE(bi-seq)
      SingleFee.CustNum     = invoice.custnum    
      SingleFee.BillTarget  = 1
      SingleFee.CalcObj     = "MINCONS_070913"
      SingleFee.BillCode    = "MCCONTR1 fixline"       
      SingleFee.BillPeriod  = 200709           /* billing Period   */
      SingleFee.Concerns[1] = 20070930         /* period concerned */
      SingleFee.Amt         = ldAmt            /* Payment          */
      SingleFee.Memo[1]     = ""
      SingleFee.Memo[2]     = ""
      SingleFee.HostTable   = "Mobsub"
      SingleFee.KeyValue    = STRING(invoice.msseq)
      SingleFee.BillType    = "SF"
      SingleFee.Contract    = ""
      SingleFee.Active      = TRUE
      SingleFee.FeeModel    = ""
      SingleFee.VATIncl     = FALSE.

   ldtot = ldtot + ldamt.
   
end.

disp i j ldtot.


