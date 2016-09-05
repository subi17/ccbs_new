/* ----------------------------------------------------------------------
  MODULE .......: UpdCustClass
  TASK .........: update CustomerClass to Customer
  APPLICATION ..: nn
  AUTHOR .......: jpo
  CREATED ......: 28-12-99
  CHANGED ......: 21.05.02/tk Event logging added
  Version ......: M15
  ---------------------------------------------------------------------- */

{Syst/commali.i}
DEF INPUT PARAMETER invDate   AS DA NO-UNDO.

{Syst/eventval.i}
IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {Func/lib/eventlog.i}

   DEFINE VARIABLE lhxxcustomer AS HANDLE NO-UNDO.
   lhxxcustomer = BUFFER customer:HANDLE.
   RUN StarEventInitialize(lhxxcustomer).

   ON F12 ANYWHERE DO:
      RUN Mc/eventview2.p(lhxxcustomer).
   END.
END.

DEF BUFFER xxCustomer FOR customer.
DEF VAR    Updated    AS  I   NO-UNDO.

FOR EACH Invoice NO-LOCK where 
         Invoice.Brand    = gcBrand AND 
         Invoice.InvDate >= invdate,
   FIRST Customer OF Invoice No-LOCK
Break 
By Invoice.CustNum.

   ACCUMULATE 
   Invoice.Amt (SUB-COUNT BY Invoice.custNum)
   Invoice.Amt (SUB-TOTAL by Invoice.CustNum).

   IF LAST-OF(Invoice.CustNum) THEN DO:

      FIND LAST CustClass USE-INDEX amt WHERE
             CustClass.Brand = gcBrand AND 
             CustClass.amt <= (ACCUM SUB-TOTAL BY invoice.CustNum Invoice.amt) /
                              (ACCUM SUB-COUNT BY invoice.CustNum Invoice.amt)
      NO-LOCK no-error.

      IF AVAIL CustClass AND CustClass.CustClass NE Customer.CustClass 
      THEN DO:  

         FIND  xxCustomer WHERE 
         recid(xxcustomer) = recid(Customer) EXCLUSIVE-LOCK.

         IF llDoEvent THEN RUN StarEventSetOldBuffer(lhxxcustomer).

         ASSIGN
         xxCustomer.CustClass = CustClass.CustClass
         updated              = updated + 1.

         IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhxxcustomer).
         PUT SCREEN ROW 1 STRING(updated).
      END.
   END.
END.

MESSAGE 
"Totally " updated " Customers updated!" 
"Totally " updated " Customers updated!" 
