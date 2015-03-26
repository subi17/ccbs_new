{testpaa.i}
katun = "ari".

{timestamp.i}
{eventval.i}

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun
   {lib/eventlog.i}

   DEFINE VARIABLE lhInvoice AS HANDLE NO-UNDO.
   lhInvoice = BUFFER Invoice:HANDLE.
   RUN StarEventInitialize(lhInvoice).

   DEFINE VARIABLE lhSingleFee AS HANDLE NO-UNDO.
   lhSingleFee = BUFFER SingleFee:HANDLE.
   RUN StarEventInitialize(lhSingleFee).
END.

def var licashcust as int no-undo.
def var i          as int no-undo.

def buffer border   for order.
def buffer binvoice for invoice.
def buffer cinvoice for invoice.
def buffer bordercustomer for ordercustomer.

for each order no-lock use-index stamp where
         order.brand = "1" and
         order.crstamp > 20080501 and
         order.crstamp < 20090101 and
         order.invnum > 0 and
         order.statuscode ne "6" and
         order.custnum = 0,
         /*
         lookup(order.statuscode,"7,73") = 0,
         */
   first ordercustomer of order no-lock where rowtype = 1,
   first binvoice no-lock where 
         binvoice.invnum = order.invnum and
         binvoice.custnum >= 400 and binvoice.custnum <= 406 and
         binvoice.invdate >= 7/1/8 and
         binvoice.invdate < 1/1/9
         /* and
         binvoice.crinvnum > 0,
   first cinvoice no-lock where
         cinvoice.invnum = binvoice.crinvnum and
         cinvoice.invdate >= 1/1/9 */:

   i = i + 1.
   pause 0.
   disp i  format ">>>>9" 
        fts2hms(order.crstamp) format "x(20)"
        binvoice.invamt
        order.statuscode
        string(order.mnpstatus > 0,"mnp/new")
        binvoice.invdate.

   RUN createcustomer(Order.OrderId, 
                      1,
                      FALSE,
                      OUTPUT liCashCust).
         
   if licashcust > 0 then DO TRANS:

      FOR EACH SingleFee EXCLUSIVE-LOCK USE-INDEX HostTable WHERE
               SingleFee.Brand     = gcBrand AND
               SingleFee.HostTable = "Order" AND
               SingleFee.KeyValue  = STRING(Order.OrderID):
               
         IF llDoEvent THEN RUN StarEventSetOldBuffer(lhSingleFee).
         SingleFee.CustNum = liCashCust.      
         IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhSingleFee).
      END.

      FIND Invoice where recid(invoice) = recid(binvoice) EXCLUSIVE-LOCK.
      IF llDoEvent THEN RUN StarEventSetOldBuffer(lhInvoice).
      Invoice.CustNum = liCashCust.
      IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhInvoice).
           
      FOR EACH Payment OF Invoice EXCLUSIVE-LOCK:
         Payment.CustNum = Invoice.CustNum.
      END.
      
      find border where recid(border) = recid(order) exclusive-lock.
      border.custnum = licashcust.
      
      find bordercustomer where recid(bordercustomer) = recid(ordercustomer)
          exclusive-lock.
      bordercustomer.custnum = licashcust.
      
   END.

   disp licashcust format ">>>>>>>9".
end.

i = 0.

for each cinvoice no-lock use-index invdate where
         cinvoice.brand = "1" and
         cinvoice.invdate >= 7/1/8 and
         cinvoice.custnum < 406,
   first binvoice no-lock where 
         binvoice.invnum = cinvoice.crinvnum:

   i = i + 1.
   pause 0.
   disp i  format ">>>>9" 
        cinvoice.invdate
        cinvoice.invtype
        cinvoice.invamt
        binvoice.invdate
        binvoice.invtype
        binvoice.custnum.

   if binvoice.custnum > 1000 then DO TRANS:

      FIND Invoice where recid(invoice) = recid(cinvoice) EXCLUSIVE-LOCK.
      IF llDoEvent THEN RUN StarEventSetOldBuffer(lhInvoice).
      Invoice.CustNum = binvoice.custnum.
      IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhInvoice).
           
      FOR EACH Payment OF Invoice EXCLUSIVE-LOCK:
         Payment.CustNum = Invoice.CustNum.
      END.
   end.
end.

fCleanEventObjects().
