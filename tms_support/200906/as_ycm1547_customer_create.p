{commpaa.i}
katun = "anttis".
gcBrand = "1".

DEFINE VARIABLE oiCustomer AS INTEGER NO-UNDO. 
DEFINE VARIABLE i AS INTEGER NO-UNDO. 

find customer where customer.custnum = 300 NO-LOCK NO-ERROR.
def stream slog.
output stream slog to /apps/snet/200906/as_ycm1547_customer_create_301.log append.
/*output stream slog to /home/anttis/test.log.*/
DEFINE VARIABLE lcInv AS CHARACTER NO-UNDO. 
DEFINE VARIABLE liCustnum AS INTEGER NO-UNDO init 301. 

FOR EACH mobsub WHERE
  brand = "1" and
  mobsub.custnum = liCustnum NO-LOCK:
  
   i = i + 1.
   /*if i > 1 then leave.*/

   find order where
    order.msseq = mobsub.msseq NO-LOCK NO-eRROR.

   find ordercustomer where
      ordercustomer.brand = "1" and
      ordercustomer.orderid = order.orderid and
      ordercustomer.rowtype = 1 NO-LOCK NO-ERROR.
      RUN /apps/snet/200906/createcustomer_ycm1547.p(INPUT Order.Orderid,1,FALSE,output oiCustomer).
   lcInv = "".
   
   FIND MsOwner WHERE
        MsOwner.MsSeq = MobSub.MsSeq NO-LOCK NO-eRROR.
   if not avail msowner then do:
      disp MobSub.msseq.
   end.

   loop:
   FOR EACH invoice where
      invoice.brand  = "1" and
      invoice.custnum = mobsub.custnum and
      invoice.invtype = 1 NO-LOCK:
      FOR EACH invrow of invoice NO-LOCK:
         if invrow.cli = mobsub.cli then do:
            lcInv = invoice.ExtInvID.
            leave loop.
         end.
      END.
   END.

/*   if mobsub.paytype = false  and mobsub.creationdate < 6/1/9 then */
   put stream slog unformatted mobsub.msseq "|" liCustnum "|" oiCustomer "|" mobsub.cli "|" mobsub.clitype "|" order.orderid "|" ordercustomer.custidtype "|" ordercustomer.custid "|"  mobsub.creationdate "|" lcInv  skip.

END.
disp i.
