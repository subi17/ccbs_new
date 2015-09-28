/* ----------------------------------------------------------------------
  MODULE .......: createcustcontact.p 
  TASK .........: Create CustContacts for Corporate orders
  APPLICATION ..: TMS
  AUTHOR .......: anttis 
  CREATED ......: 04.02.09
  CHANGED ......:
  Version ......: xfera
----------------------------------------------------------------------- */
{commali.i} 
{tmsconst.i}
{eventval.i}
IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun
   {lib/eventlog.i}
   DEFINE VARIABLE lhCustContact AS HANDLE NO-UNDO.
END.

DEF INPUT PARAM iiOrderId LIKE Order.OrderId.
DEF INPUT PARAM iiCustNum LIKE Customer.Custnum.
DEF INPUT PARAM iiRowType AS INTEGER NO-UNDO.
DEF OUTPUT PARAM ocError AS CHAR NO-UNDO.

FIND Order NO-LOCK WHERE
   Order.Brand   = gcBrand AND
   Order.OrderId = iiOrderId 
NO-ERROR.

IF NOT AVAIL Order THEN DO:
   ocError = "ERROR: Order not found".
   RETURN.
END.

FIND OrderCustomer NO-LOCK WHERE
   OrderCustomer.Brand   = gcBrand AND
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

FIND CustContact WHERE 
   CustContact.Brand = gcBrand AND
   CustContact.Custnum = Customer.CustNum AND
   CustContact.CustType = iiRowType EXCLUSIVE-LOCK NO-ERROR.

IF lldoevent then do:
   lhcustcontact = buffer custcontact:handle.
   RUN StarEventInitialize(lhCustContact).
END.

IF NOT AVAIL CustContact THEN DO:
   CREATE CustContact.
END.
ELSE IF lldoevent THEN RUN StarEventSetOldBuffer(lhCustContact).

ASSIGN
   CustContact.Brand          = gcBrand 
   CustContact.Custnum        = iiCustnum
   CustContact.CustType       = iiRowType
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
   ELSE RUN StarEventMakeModifyEvent (lhCustContact).
END.

fCleanEventObjects().

RELEASE CustContact. 
