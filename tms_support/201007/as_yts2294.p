
DEFINE VARIABLE ldInvDate AS DATE NO-UNDO. 
DEFINE VARIABLE ldLimitDate AS DATE NO-UNDO.

DEFINE STREAM sout.

ldInvDate = DATE(7,1,2010).
ldLimitDate = DATE(4,30,2010).
OUTPUT STREAM sout TO "yts-2294_v3.csv".

DEFINE VARIABLE i AS INTEGER NO-UNDO. 
DEFINE VARIABLE lcMinusRow AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcBillPerm AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcInterval AS CHARACTER NO-UNDO. 

def buffer  bMinusRow FOR InvRow.

INVOICE_LOOP:
FOR EACH Invoice NO-LOCK WHERE
         Invoice.Brand = "1" AND
         Invoice.InvType = 1 AND
         Invoice.InvDate = ldInvDate:


   FOR EACH InvRow NO-LOCK WHERE
         InvRow.InvNum = Invoice.InvNum AND
         InvRow.FromDate <= ldLimitDate :

   lcMinusRow = "".
   if can-find(first invrow where
                     invrow.invnum = invoice.invnum and
                     invrow.amt < 0 NO-LOCK) then do:
      FOR EACH bMinusRow NO-LOCK WHERE
               bMinusRow.invnum = invoice.invnum and
               bMinusRow.amt < 0:
         lcMinusRow = lcMinusRow  + ", " + bMinusRow.billcode + ": " +  string(bMinusRow.amt).
      END.
      lcMinusRow = SUBSTRING(lcMinusRow,3).
   end.
   else lcMinusRow = "".

   find subinvoice where
        subinvoice.invnum = invrow.invnum and
        subinvoice.subinvnum = invrow.subinvnum NO-LOCK NO-eRROR.


   IF AVAIL subinvoice then do:

      lcBillPerm = "".
      FOR EACH limit where
               limit.msseq = subinvoice.msseq and
               limit.limittype = 3 NO-LOCK:

         lcBillPerm = lcBillPerm + ", " +
               string(limit.limitamt) + ": " +
               string(limit.fromdate, "99-99-9999") + "-" +
               string(limit.todate, "99-99-9999").
      END.
      lcBillPerm = SUBSTRING(lcBillPerm,2).
   END.
  
   PUT STREAM sout UNFORMATTED 
          Invoice.ExtInvID  + ";" +
          STRING(Invoice.CustNum)   + ";" +
          InvRow.CLI        + ";" + 
	   InvRow.BillCode	+ ";" +
	   STRING(InvRow.Amt)	+ ";" +
	   STRING(InvRow.Qty)	+ ";" +	
          STRING(InvRow.FromDate)   + ";" +
          STRING(InvRow.ToDate) ";"
          lcMinusRow + ";"
          lcBillPerm  SKIP. 
   END.
   i = i + 1.
   if i mod 50000 = 0 then disp i.
END.

OUTPUT STREAM sout CLOSE.

 
