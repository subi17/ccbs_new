/**
 * Relaunches MNP process.
 *
 * @input   order_id;int;mandatory;Order ID
            old_operator;string;manadatory;Mobile operator name
            old_icc;string;mandatory;can be empty for postpaid
            id_type;string;mandatory;Customer ID Type
            customer_id;string;mandatory;Customer ID
            company;string;(mandatory);company name (for cif customers)
            first_name;string;(mandatory);first name (for normal customer)
            surname1;string;(mandatory);1st surname (for normal customer)
            surname2;string;(mandatory);2nd surname (for normal customer)
            username;string;mandatory;creator of the changes
            memo;struct;mandatory;
 * @memo    title;string;mandatory;
            content;string;mandatory
 * @output  success;boolean
 */

{fcgi_agent/xmlrpc/xmlrpc_access.i} 
{Syst/commpaa.i}
Syst.Var:gcBrand = "1".
{Syst/tmsconst.i}
{Func/orderfunc.i}
{Func/fcustdata.i}
{Syst/eventval.i}

/* Input parameters */
DEF VAR pcBrand       AS CHAR NO-UNDO.
DEF VAR piOrderId     AS INT  NO-UNDO.
DEF VAR pcOldOperator AS CHAR NO-UNDO.
DEF VAR pcOldICC      AS CHAR NO-UNDO.
DEF VAR pcIDType      AS CHAR NO-UNDO.
DEF VAR pcCustomerID  AS CHAR NO-UNDO.
DEF VAR pcCompany     AS CHAR NO-UNDO.
DEF VAR pcFirstName   AS CHAR NO-UNDO.
DEF VAR pcSurname1    AS CHAR NO-UNDO.
DEF VAR pcSurname2    AS CHAR NO-UNDO.
DEF VAR pcCreator     AS CHAR NO-UNDO.
DEF VAR pcStruct      AS CHAR NO-UNDO.
DEF VAR lcStruct      AS CHAR NO-UNDO.
DEFINE VARIABLE llMobileHolder  AS LOGICAL NO-UNDO.
DEFINE VARIABLE lhOrderCustomer AS HANDLE  NO-UNDO.
DEFINE VARIABLE llDataUpdated   AS LOGICAL NO-UNDO.

DEF VAR pcTermStruct AS CHAR NO-UNDO.
DEF VAR lcTermStruct AS CHAR NO-UNDO.

DEF VAR pcMemoStruct AS CHARACTER NO-UNDO. 
DEF VAR pcMemoTitle AS CHARACTER NO-UNDO. 
DEF VAR pcMemoContent AS CHARACTER NO-UNDO. 

/* Output parameters */
DEF VAR result AS LOGICAL.

IF validate_request(param_toplevel_id,"string,struct") = ? THEN RETURN.

pcBrand = get_string(param_toplevel_id,"0").
pcStruct = get_struct(param_toplevel_id,"1").

lcstruct = validate_struct(pcStruct, "order_id!,old_operator!,old_icc!,id_type!,customer_id!,username!,company,first_name,surname1,surname2,memo!").

IF gi_xmlrpc_error NE 0 THEN RETURN.

piOrderID     = get_int(pcStruct,"order_id").
pcOldOperator = get_string(pcStruct,"old_operator").
pcOldICC      = get_string(pcStruct,"old_icc").
pcIDType      = get_string(pcStruct,"id_type").
pcCustomerID  = get_string(pcStruct,"customer_id").
pcCreator     = "VISTA_" + get_string(pcStruct,"username").

IF TRIM(pcCreator) EQ "VISTA_" THEN RETURN appl_err("username is empty").

Syst.Var:katun = pcCreator.

IF pcIDType = "CIF" THEN
   pcCompany     = get_string(pcStruct,"company").
ELSE 
   ASSIGN
       pcFirstName = get_string(pcStruct,"first_name")
       pcSurname1  = get_string(pcStruct,"surname1")
       pcSurname2  = get_string(pcStruct,"surname2") WHEN LOOKUP("surname2",lcStruct) > 0.

pcMemoStruct = get_struct(pcStruct,"memo").
pcMemoTitle = get_string(pcMemoStruct,"title").
pcMemoContent = get_string(pcMemoStruct,"content").

IF gi_xmlrpc_error NE 0 THEN RETURN.

DO ON ERROR UNDO, THROW:
   multitenancy.TenantInformation:mSetEffectiveBrand(pcBrand).

   CATCH errorobj AS Progress.Lang.AppError:
      RETURN appl_err(errorobj:GetMessage(1)).
   END.
END.

/* validation starts */
FIND FIRST Order WHERE Order.Brand = "1" AND Order.OrderId = piOrderId EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
IF LOCKED Order THEN 
   RETURN appl_err("Order record is locked!").
IF NOT AVAIL Order THEN 
   RETURN appl_err(SUBST("Order &1 not found!", piOrderId)). 

FIND OrderCustomer OF Order NO-LOCK WHERE
   OrderCustomer.RowType = {&ORDERCUSTOMER_ROWTYPE_MOBILE_POUSER} NO-ERROR.

IF AVAILABLE OrderCustomer
THEN llMobileHolder = TRUE.
ELSE FIND OrderCustomer OF Order NO-LOCK WHERE
      OrderCustomer.RowType = {&ORDERCUSTOMER_ROWTYPE_AGREEMENT} NO-ERROR.

IF llMobileHolder = FALSE AND NOT AVAILABLE OrderCustomer
THEN RETURN appl_err("Neither mobile holder nor agreement order customer found!").

IF Order.StatusCode NE "73" THEN
   RETURN appl_err("Cannot create new MNP process with order status " + 
   Order.StatusCode).

FIND MNPProcess WHERE
     MNPProcess.OrderID = Order.OrderID AND
     MNPProcess.MNPType = {&MNP_TYPE_IN} AND
     MNPProcess.Statuscode EQ ({&MNP_ST_AREC}) NO-LOCK NO-ERROR.

IF NOT AVAIL MNPProcess THEN DO:
   RETURN appl_err("Cannot find MNP process with rejected status").
END.

IF Order.OldPayType AND pcOldICC EQ "" THEN DO:
   RETURN appl_err("ICC is missing").
END.

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER Syst.Var:katun
   {Func/lib/eventlog.i}
END.

IF Order.CurrOper NE pcOldOperator THEN DO:
   
   FIND FIRST MNPOperator WHERE
              MNPOperator.Brand = Syst.Var:gcBrand AND
              MNPOperator.OperName = pcOldOperator
   NO-LOCK NO-ERROR.
   IF NOT AVAIL MNPOperator
   THEN RETURN appl_err(SUBST("Unknown operator &1",pcOldOperator)).

END.

FIND FIRST TMSCodes WHERE
           TMSCodes.TableName = "Customer" AND
           TMSCodes.FieldName = "CustIdType" AND
           TMSCodes.CodeValue = pcIdType NO-LOCK NO-ERROR.
IF NOT AVAIL TMSCodes
THEN RETURN appl_err(SUBST("Unknown ID type &1", pcIdType)).

IF OrderCustomer.Custid NE pcCustomerID OR
   OrderCustomer.CustidType NE pcIDType
THEN DO:
   IF (OrderCustomer.CustidType NE pcIDType) AND
      (OrderCustomer.CustidType EQ "CIF" OR pcIDType EQ "CIF")
   THEN RETURN appl_err("ID type change is not allowed").

   IF NOT fChkCustID(pcIDType,pcCustomerId)
   THEN RETURN appl_err("Invalid customer ID").
END.

IF NOT llMobileHolder
THEN DO:
   DEFINE BUFFER lbMobileHolder FOR OrderCustomer.
   lhOrderCustomer = BUFFER lbMobileHolder:HANDLE.
   IF llDoEvent THEN RUN StarEventInitialize(lhOrderCustomer).

   CREATE lbMobileHolder.
   
   BUFFER-COPY OrderCustomer
      USING
         OrderCustomer.Brand
         OrderCustomer.OrderId
         OrderCustomer.BankCode
         OrderCustomer.Pro
         OrderCustomer.Language
         OrderCustomer.CustDataRetr
         OrderCustomer.MSISDNForIdent
         OrderCustomer.DelType
      TO
         lbMobileHolder
      ASSIGN
         lbMobileHolder.DataChecked = ?
         lbMobileHolder.RowType     = {&ORDERCUSTOMER_ROWTYPE_MOBILE_POUSER}
         lbMobileHolder.CustId      = pcCustomerID
         lbMobileHolder.CustIdType  = pcIDType.

    IF pcIDType = "CIF"
    THEN lbMobileHolder.Company = pcCompany.
    ELSE ASSIGN
           lbMobileHolder.FirstName = pcFirstName
           lbMobileHolder.Surname1  = pcSurname1
           lbMobileHolder.Surname2  = pcSurname2 WHEN LOOKUP("surname2",lcStruct) > 0.
           .
    IF llDoEvent THEN RUN StarEventMakeCreateEvent (lhOrderCustomer).

    llDataUpdated = TRUE.
END.
ELSE IF OrderCustomer.Custid     NE pcCustomerID OR
        OrderCustomer.CustidType NE pcIDType     OR
        ( pcIDType EQ "CIF" AND OrderCustomer.Company NE pcCompany ) OR
        ( pcIDType NE "CIF" AND (OrderCustomer.FirstName NE pcFirstName OR
                                 OrderCustomer.Surname1  NE pcSurname1 OR
                                 OrderCustomer.Surname2  NE pcSurname2 ) )
THEN DO:
   FIND CURRENT OrderCustomer EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
   IF LOCKED(OrderCustomer)
   THEN RETURN appl_err("OrderCustomer record is locked").

   IF llDoEvent THEN DO:
      lhOrderCustomer = BUFFER OrderCustomer:HANDLE.
      RUN StarEventInitialize(lhOrderCustomer).
      RUN StarEventSetOldBuffer(lhOrderCustomer).
   END.

   ASSIGN 
      OrderCustomer.CustId     = pcCustomerID
      OrderCustomer.CustIdType = pcIDType.
      
    IF pcIDType = "CIF" THEN
      OrderCustomer.Company = pcCompany.
    ELSE ASSIGN
      OrderCustomer.FirstName = pcFirstName
      OrderCustomer.Surname1  = pcSurname1
      OrderCustomer.Surname2  = pcSurname2 WHEN LOOKUP("surname2",lcStruct) > 0.

   IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhOrderCustomer).
   RELEASE OrderCustomer.

   llDataUpdated = TRUE.

END.

IF llDoEvent THEN DO:
   DEFINE VARIABLE lhOrder AS HANDLE NO-UNDO.
   lhOrder = BUFFER Order:HANDLE.
   RUN StarEventInitialize(lhOrder).
   RUN StarEventSetOldBuffer(lhOrder).
END.

ASSIGN
   Order.CurrOper = pcOldOperator
   Order.OldIcc   = pcOldICC.

fSetOrderStatus(Order.OrderId,"3").

IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhOrder).
FIND CURRENT Order NO-LOCK.

CREATE Memo.
ASSIGN
    Memo.CreStamp  = {&nowTS}
    Memo.Brand     = Syst.Var:gcBrand
    Memo.HostTable = "Order"
    Memo.KeyValue  = STRING(Order.OrderId)
    Memo.MemoSeq   = NEXT-VALUE(MemoSeq)
    Memo.CreUser   = pcCreator
    Memo.MemoTitle = pcMemoTitle
    Memo.MemoText  = pcMemoContent +
                     ( IF llDataUpdated
                       THEN CHR(10) + "Relanzamiento de portabilidad debido a datos incorrectos del cliente"
                       ELSE "" )
    Memo.CustNum   = Order.Custnum.

IF AVAIL MNPProcess THEN DO:

   CREATE Memo.
   ASSIGN
       Memo.CreStamp  = {&nowTS}
       Memo.Brand     = Syst.Var:gcBrand
       Memo.HostTable = "MNPProcess"
       Memo.KeyValue  = STRING(MNPProcess.MNPSeq)
       Memo.MemoSeq   = NEXT-VALUE(MemoSeq)
       Memo.CreUser   = pcCreator
       Memo.MemoTitle = pcMemoTitle
       Memo.MemoText  = pcMemoContent.
END.

add_boolean(response_toplevel_id, "", true).

FINALLY:
   IF llDoEvent
   THEN fCleanEventObjects().
END.
