/* ----------------------------------------------------------------------
  MODULE .......: createcustcontact.p
  TASK .........: Create CustContacts for Corporate orders
  APPLICATION ..: TMS
  AUTHOR .......: anttis
  CREATED ......: 04.02.09
  CHANGED ......:
  Version ......: xfera
----------------------------------------------------------------------- */
{Syst/commali.i}
{Syst/tmsconst.i}
{Syst/eventval.i}
IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER Syst.Var:katun
   {Func/lib/eventlog.i}
   DEFINE VARIABLE lhCustContact AS HANDLE NO-UNDO.
   
   lhcustcontact = buffer custcontact:handle.
   RUN StarEventInitialize(lhCustContact).
END.

DEF INPUT PARAM iiOrderId LIKE Order.OrderId.
DEF INPUT PARAM iiCustNum LIKE Customer.Custnum.
DEF INPUT PARAM iiRowType AS INTEGER NO-UNDO.
DEF OUTPUT PARAM ocError AS CHAR NO-UNDO.

DEF VAR llUpdateCustContact AS LOG  NO-UNDO INIT FALSE.
DEF VAR lcMemo              AS CHAR NO-UNDO.

DEF BUFFER bMobSub FOR MobSub.
DEF BUFFER bOrderCustomer FOR OrderCustomer.

FIND Order NO-LOCK WHERE
   Order.Brand   = Syst.Var:gcBrand AND
   Order.OrderId = iiOrderId
NO-ERROR.

IF NOT AVAIL Order THEN DO:
   ocError = "ERROR: Order not found".
   RETURN.
END.

FIND OrderCustomer NO-LOCK WHERE
   OrderCustomer.Brand   = Syst.Var:gcBrand AND
   OrderCustomer.OrderId = iiOrderId AND
   OrderCustomer.RowType = iiRowType NO-ERROR.

IF NOT AVAIL OrderCustomer THEN DO:
   ocError = "ERROR: OrderCustomer not found".
   RETURN.
END.

FIND Customer NO-LOCK WHERE
   Customer.Custnum = iiCustnum NO-ERROR.
IF NOT AVAIL Customer THEN DO:
   ocError = SUBST("ERROR: Customer &1 not found", iiCustnum).
   RETURN.
END.

lcMemo = "Order" + CHR(255) +
          STRING(Customer.CustNum) + CHR(255) +
          STRING(Order.OrderId) + CHR(255) +
          Order.Salesman + CHR(255) +
          Order.OrderChannel.

IF Order.OrderType < 2 THEN DO:
   
   IF Order.PayType = FALSE AND
      NOT CAN-FIND(FIRST bMobSub WHERE
                         bMobSub.Brand     = Syst.Var:gcBrand AND
                         bMobSub.CustNum   = Customer.CustNum AND
                         bMobSub.MsSeq    <> Order.MsSeq AND
                         bMobSub.PayType   = FALSE) THEN
      llUpdateCustContact = TRUE.
   
   IF Order.PayType = TRUE AND
      NOT CAN-FIND(FIRST bMobSub WHERE
                         bMobSub.Brand     = Syst.Var:gcBrand AND
                         bMobSub.CustNum   = Customer.CustNum AND
                         bMobSub.MsSeq    <> Order.MsSeq) 
                         AND
      NOT CAN-FIND(FIRST TermMobSub WHERE
                         TermMobSub.Brand     = Syst.Var:gcBrand AND
                         TermMobSub.CustNum   = Customer.CustNum AND
                         TermMobSub.MsSeq    <> Order.MsSeq)  THEN
      llUpdateCustContact = TRUE.

END.
ELSE llUpdateCustContact = TRUE.

IF llUpdateCustContact THEN DO:

   IF OrderCustomer.Rowtype = {&ORDERCUSTOMER_ROWTYPE_AGREEMENT} THEN DO: 
      IF NOT CAN-FIND(FIRST bOrderCustomer WHERE
                            bOrderCustomer.Brand   = Syst.Var:gcBrand AND
                            bOrderCustomer.OrderId = OrderCustomer.OrderId AND
                            bOrderCustomer.RowType = {&ORDERCUSTOMER_ROWTYPE_CIF_CONTACT}) THEN DO:
         FIND CustContact WHERE
              CustContact.Brand    = Syst.Var:gcBrand AND
              CustContact.Custnum  = Customer.CustNum AND
              CustContact.CustType = {&ORDERCUSTOMER_ROWTYPE_CIF_CONTACT}
              EXCLUSIVE-LOCK NO-ERROR.
         IF AVAILABLE CustContact THEN DO:
            IF llDoEvent THEN RUN StarEventMakeDeleteEventWithMemo(
                                    lhCustContact,
                                    Syst.Var:katun,
                                    lcMemo).
            DELETE CustContact.
         END.
      END.
   END.
   ELSE DO:
      FIND CustContact WHERE
           CustContact.Brand = Syst.Var:gcBrand AND
           CustContact.Custnum = Customer.CustNum AND
           CustContact.CustType = {&ORDERCUSTOMER_ROWTYPE_CIF_CONTACT}
           EXCLUSIVE-LOCK NO-ERROR.
      
      IF NOT AVAIL CustContact THEN DO:
         CREATE CustContact.
      END.
      ELSE IF lldoevent THEN RUN StarEventSetOldBuffer(lhCustContact).

      ASSIGN
         CustContact.Brand          = Syst.Var:gcBrand
         CustContact.Custnum        = iiCustnum
         CustContact.CustType       = {&ORDERCUSTOMER_ROWTYPE_CIF_CONTACT}
         CustContact.HonTitle       = OrderCustomer.CustTitle
         CustContact.FirstName      = OrderCustomer.FirstName
         CustContact.CustName       = OrderCustomer.Surname1
         CustContact.Surname2       = OrderCustomer.Surname2
         CustContact.CustIdType     = OrderCustomer.CustIdType
         CustContact.OrgId          = OrderCustomer.Custid
         CustContact.Nationality    = OrderCustomer.Nationality
         CustContact.Language       = INT(OrderCustomer.Language)
         CustContact.SMSNumber      = OrderCustomer.MobileNumber
         CustContact.Email          = OrderCustomer.Email
         CustContact.DirMarkSMS     = OrderCustomer.OperSMSMarketing
         CustContact.DirMarkEmail   = OrderCustomer.OperEmailMarketing
         CustContact.DirMarkPost    = OrderCustomer.OperPostMarketing
         CustContact.OutMarkSMS     = OrderCustomer.OutSMSMarketing
         CustContact.OutMarkEmail   = OrderCustomer.OutEmailMarketing
         CustContact.OutMarkPost    = OrderCustomer.OutpostMarketing
         CustContact.Address        = OrderCustomer.Address
         CustContact.ZipCode        = OrderCustomer.ZipCode
         CustContact.PostOffice     = OrderCustomer.PostOffice
         CustContact.Region         = OrderCustomer.Region
         CustContact.AddressCodC    = OrderCustomer.AddressCodC
         CustContact.AddressCodP    = OrderCustomer.AddressCodP
         CustContact.AddressCodM    = OrderCustomer.AddressCodM.

      IF llDoEvent THEN DO:
         IF NEW CustContact THEN RUN StarEventMakeCreateEvent (lhCustContact).
         ELSE RUN StarEventMakeModifyEventWithMemo(
                     lhCustContact,
                     Syst.Var:katun,
                     lcMemo).
      END.

      RELEASE CustContact.

   END.

END. /* IF llUpdateCustContact THEN DO: */

fCleanEventObjects().
