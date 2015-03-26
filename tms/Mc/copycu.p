/* ----------------------------------------------------------------------------
  MODULE .......: copycu.p 
  FUNCTION .....: Copy a customer record
  APPLICATION ..: NN
  AUTHOR .......: PT
  CREATED ......: 21.12.00 pt
  CHANGED ......: 26.01.01 pt set 4 additional cust.nos as "new-Custno"
                  15.03.02 jp debug - mode
                  09.10.02 jr Use numbers series
                  28.03.03/aam use of number series corrected,
                               eventlog
                  16.05.03/aam copy BillTargets,
                               lhCustomer must point to new-customer
                  12.09.03/aam brand
                  01.10.03/tk  ask what to copy,
                               copy PNP numbers + Customer's prices
                  23.01.04/aam AgrCust             
                  24.01.06/jt  DYNAMIC-FUNCTION("fDispCustName",
                  
  Version ......: M15
  --------------------------------------------------------------------------- */

{commali.i}
{eventval.i}

DEF BUFFER new-Customer FOR Customer.
DEF BUFFER new-Target   FOR BillTarget. 
DEF BUFFER new-Tariff   FOR Tariff.
DEF BUFFER new-PnpList  FOR PNPList.

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {lib/eventlog.i}

   DEFINE VARIABLE lhCustomer AS HANDLE NO-UNDO.
   lhCustomer = BUFFER new-Customer:HANDLE.
   RUN StarEventInitialize(lhCustomer).

   DEFINE VARIABLE lhBillTarget AS HANDLE NO-UNDO.
   lhBillTarget = BUFFER new-Target:HANDLE.
   RUN StarEventInitialize(lhBillTarget).

END.


DEF INPUT-OUTPUT PARAMETER  liCustNum LIKE Customer.CustNum NO-UNDO.
DEF INPUT        PARAMETER  debug     AS LOGICAL            NO-UNDO.

DEF NEW SHARED VAR siirto AS CHAR.

DEF VAR ok         AS LOG NO-UNDO.
DEF VAR new-Custno AS INT NO-UNDO.
DEF VAR i          AS INT NO-UNDO.
DEF VAR x1         AS INT NO-UNDO.
DEF VAR x2         AS INT NO-UNDO. 

DEF VAR CopyBT     AS LOG NO-UNDO.
DEF VAR CopyPNP    AS LOG NO-UNDO.
DEF VAR CopyPrices AS LOG NO-UNDO.
DEF VAR lcCustName AS CHAR NO-UNDO.

FORM
   CopyBT     label "Billing Targets." skip
   CopyPNP    label "PNP Numbers....." skip
   CopyPrices label "Private Prices.." skip
WITH CENTERED OVERLAY ROW 10 SIDE-LABELS TITLE " SELECT ITEMS TO COPY "
FRAME valinta.   

FIND Customer WHERE Customer.CustNum = liCustNum NO-LOCK NO-ERROR.

lcCustName = DYNAMIC-FUNCTION("fDispCustName" IN ghFunc1,
                              BUFFER Customer).
                                    
PAUSE 0.

ok = FALSE.
IF DEBUG THEN 
MESSAGE 
   "Are You sure You want to copy" SKIP
   "customer no." Customer.CustNum lcCustName "?"
   VIEW-AS ALERT-BOX BUTTONS YES-NO UPDATE ok.
ELSE ok = TRUE.

IF ok THEN DO:

    VIEW FRAME valinta.
    UPDATE 
       CopyBT
       CopyPNP
       CopyPrices
    WITH FRAME valinta.
    pause.

   /* get next free number */
   RUN custser.

   i = index(siirto,"-").
   IF i > 0 THEN DO:
      ASSIGN x1 = int(substr(siirto, 1, i - 1))
             x2 = int(substr(siirto, i + 1)).

      FIND LAST new-Customer NO-LOCK WHERE
                new-Customer.Brand    = gcBrand AND
                new-Customer.CustNum >= x1      AND
                new-Customer.CustNum <= x2 NO-ERROR.
      IF AVAILABLE new-Customer THEN ASSIGN
         x1 = new-Customer.CustNum. 

      DO new-custNo = x1 TO x2:
         IF NOT can-find(FIRST new-Customer where
                               new-Customer.Brand   = gcBrand AND
                               new-Customer.CustNum = new-custNo) THEN LEAVE.
      END.
   END.
   ELSE DO:
      FIND LAST new-Customer NO-LOCK USE-INDEX CustNum WHERE
                new-Customer.Brand  = gcBrand.
      new-custNo = new-Customer.CustNum + 1.
   END.

   CREATE new-Customer.
   ASSIGN new-Customer.CustNum = new-Custno.
   BUFFER-COPY Customer EXCEPT CustNum TO new-Customer.

   ASSIGN
      new-Customer.RateCust  = new-Custno
      new-Customer.InvCust   = new-Custno
      new-Customer.PaymCust  = new-Custno
      new-Customer.RepCust   = new-Custno
      new-Customer.AgrCust   = new-Custno.

   IF llDoEvent THEN RUN StarEventMakeCreateEvent(lhCustomer).

   IF CopyBT THEN DO:
      FOR EACH BillTarget OF Customer NO-LOCK:
         CREATE new-Target.
         BUFFER-COPY BillTarget EXCEPT CustNum TO new-Target.
         ASSIGN new-Target.CustNum    = new-Customer.CustNum.
      END.
   END.

   IF CopyPNP THEN DO:
      FOR EACH PNPList OF Customer NO-LOCK:
         CREATE new-PNPList.
         BUFFER-COPY PNPList EXCEPT CustNum TO new-PNPList.
         ASSIGN new-PNPList.CustNum   = new-Customer.CustNum.
      END.
   END.

   IF CopyPrices THEN DO:
      FOR EACH Tariff OF Customer NO-LOCK.
         CREATE new-Tariff.
         BUFFER-COPY Tariff EXCEPT CustNum TariffNum TO new-Tariff.
         ASSIGN 
            new-Tariff.TariffNum = NEXT-VALUE(Tariff)
            new-Tariff.CustNum = new-Customer.CustNum.
      END.             
   END.             

   IF debug THEN
   MESSAGE "COPY COMPLETED - NEW CUSTOMER GOT NO." new-Custno
   VIEW-AS ALERT-BOX.
   liCustNum = new-Custno.
END.


