{Func/timestamp.i}

def buffer brow for invrow.

def var i as int no-undo.
def var j as int no-undo.
def var k as int no-undo.

def var ldtdate as date no-undo.
def var litime  as int  no-undo.

def stream slog.
output stream slog to /apps/snet/200801/aam_yts459_found.log.

put stream slog unformatted
   "MSISDN"   chr(9)
   "Subs.ID"  chr(9)
   "Order"    chr(9)
   "Time"     chr(9)
   "BillItem" chr(9)
   "Qty"      skip.
   
for each invoice no-lock use-index invdate where
         invoice.brand = "1" and
         invoice.invdate >= 1/29/8 and
         lookup(string(invoice.invtype),"6,7") > 0,
   first order no-lock where
         order.invnum = invoice.invnum
         /* ,
   first sim no-lock where
         sim.icc = order.icc and
         sim.simstat = 21
         */
by chgstamp:

   i = i + 1.
   
   j = 0.
   
   for each invrow of invoice no-lock where
            invrow.amt >= 0,
      first billitem no-lock where 
            billitem.brand = "1" and
            billitem.billcode = invrow.billcode and
            billitem.bigroup = "7":
           
      j = j + invrow.qty.
   end.
   
   if j > 1 then do:

      for each invrow of invoice no-lock,
         first billitem no-lock where 
               billitem.brand = "1" and
               billitem.billcode = invrow.billcode and
               billitem.bigroup = "7":
          
         put stream slog unformatted
            order.cli     chr(9)
            order.msseq   chr(9)
            order.orderid chr(9)
            fts2hms(invoice.chgstamp) chr(9)
            invoice.extinvid chr(9)
            invoice.invnum   chr(9)
            invrow.billcode chr(9)
            invrow.qty      skip.
      end.
      
      k = k + 1.
   end.
   
   pause 0.
   disp i k with 1 down.

end.

