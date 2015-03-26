/* ----------------------------------------------------------------------------
  MODULE .......: copycu.p 
  FUNCTION .....: Copy a customer record
  APPLICATION ..: NN
  AUTHOR .......: PT
  CREATED ......: 21.12.00 pt
  CHANGED ......: 26.01.01 pt set 4 additional cust.nos as "new-no"
                  15.03.02 jp debug - mode
                  09.10.02 jr Use numbers series
                  15.09.03 jp Brand
                  26.01.04 tk agrcust
                  23.03.04 aam check if customer exists (new-no)
                  18.06.04 tk  creuser and credate
                  10.01.06 aam clean eventlog
                  24.01.06 jt DYNAMIC-FUNCTION("fDispCustName"  Version ......: M15
--------------------------------------------------------------------------- */

{commali.i}

DEF BUFFER new-Customer FOR Customer.

{eventval.i}
IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {lib/eventlog.i}

   DEFINE VARIABLE lhCustomer AS HANDLE NO-UNDO.
   lhCustomer = BUFFER new-Customer:HANDLE.
   RUN StarEventInitialize(lhCustomer).

END.


DEF INPUT-OUTPUT PARAMETER   CustNum LIKE Customer.CustNum .
DEF INPUT        PARAMETER   debug  AS LOGICAL NO-UNDO .
DEF VAR ok                          AS LO NO-UNDO.
DEF VAR new-no                      AS I  NO-UNDO.
DEF VAR lcCustName                  AS CHAR NO-UNDO.

FIND Customer WHERE Customer.CustNum = CustNum NO-LOCK NO-ERROR.
PAUSE 0.

lcCustName = DYNAMIC-FUNCTION("fDispCustName" IN ghFunc1,
                              BUFFER Customer).
                                    
ok = FALSE.
IF DEBUG THEN 
MESSAGE 
   "Are You SURE You want to copy" SKIP
   "customer no." CustNum lcCustName 
   VIEW-AS ALERT-BOX BUTTONS YES-NO UPDATE ok.
ELSE ok = TRUE.

IF ok THEN DO:
   
   FIND LAST new-Customer WHERE
      new-Customer.Brand = gcBrand
   USE-INDEX CustNum NO-LOCK NO-ERROR.
   
   new-no = new-Customer.Custnum + 1.
   
   CREATE new-Customer.
   
   REPEAT:
      new-Customer.CustNum = new-no NO-ERROR.
      
      VALIDATE new-Customer NO-ERROR.

      IF ERROR-STATUS:ERROR OR new-Customer.Custnum = 0 THEN DO:
         new-no = new-no + 1.
         NEXT.
      END.
      ELSE LEAVE.
   END.
   
   BUFFER-COPY Customer EXCEPT CustNum TO new-Customer.

   ASSIGN
      new-Customer.RateCust  = new-no
      new-Customer.InvCust   = new-no
      new-Customer.PaymCust  = new-no
      new-Customer.RepCust   = new-no
      new-Customer.AgrCust   = new-no
      new-customer.Brand     = gcBrand
      new-customer.CreUser   = katun
      new-customer.CreDate   = TODAY.


   IF llDoEvent THEN RUN StarEventMakeCreateEvent(lhCustomer).

   IF debug THEN
      MESSAGE "COPY COMPLETED - NEW CUSTOMER GOT NO." new-no
      VIEW-AS ALERT-BOX.
   
   CustNum = new-no.
END.

/* clean eventlog */
fCleanEventObjects().



