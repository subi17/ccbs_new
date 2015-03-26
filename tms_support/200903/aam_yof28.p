def buffer bfmitem for fmitem.

for each fmitem no-lock where
         fmitem.brand   = "1" and
         fmitem.todate >= 3/1/9 and
         /*
         fmitem.fromdate ne 3/4/9 and
         */
         fmitem.feemodel begins "pre" and
         lookup(fmitem.feemodel,"preactpos") = 0,
   first billitem no-lock where
         billitem.brand = "1" and
         billitem.billcode = fmitem.billcode and
         lookup(billitem.bigroup,"10,12") > 0:

   disp fmitem.feemodel format "x(12)"
        fmitem.billcode
        fmitem.fromdate
        fmitem.todate
        fmitem.amount.

   /*
   if fmitem.fromdate = 3/4/9 then next.

   create bfmitem.
   buffer-copy fmitem except fromdate to bfmitem.
   assign
      bfmitem.fromdate = 3/4/9  
      bfmitem.todate = 12/31/49.
   
   if index(fmitem.feemodel,"sim") > 0 
   then bfmitem.amount = 20.
   else bfmitem.amount = 5.
   
   if fmitem.amount < 0 then bfmitem.amount = bfmitem.amount * -1.
   
   find bfmitem where recid(bfmitem) = recid(fmitem) exclusive-lock.
   bfmitem.todate = 3/3/9.
   */
end.
