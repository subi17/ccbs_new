/* ----------------------------------------------------------------------
  MODULE .......: createcustomer.p
  TASK .........: create new customer from order
  CREATED ......: JP
  CHANGED ......: 20.11.06/aam new db structure
                  12.04.07/aam update old customer's bank data
                  23.07.2015 hugo.lujan YPR-1949 [DCH] TMS - Residential 
                  customer information is updated when handling a new/mnp 
                  order and order is in status 6 
  Version ......: yoigo
-------------------------------------------------------------------------- */
{commali.i} 
{timestamp.i}
{cparam2.i}
{eventval.i}
{forderstamp.i}
{tmsconst.i}
{order.i}
{fcustdata.i}

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun
   {lib/eventlog.i}
END.

DEF INPUT  PARAMETER  iiOrderId  LIKE Order.OrderId.
DEF INPUT  PARAMETER  iiRole     AS   INT.
DEF INPUT  PARAMETER  ilDisp     AS   LOG.
DEF OUTPUT PARAMETER  oiCustNum  AS   INT.

DEF VAR new-CustNum  AS I    NO-UNDO.
DEF VAR lcCustName   AS CH   NO-UNDO.            
DEF VAR liAgrCust    AS I    NO-UNDO.
DEF VAR liInvCust    AS I    NO-UNDO.
DEF VAR llOldCust    AS log  NO-UNDO.
def var liOldCustNum AS I    NO-UNDO.
DEF VAR llCreateCust AS L    NO-UNDO.
DEF VAR ocOrdCust    AS CHAR NO-UNDO.
DEF VAR liTarget     AS INT  NO-UNDO.
DEF VAR lcInvGroup   AS CHAR NO-UNDO.
DEF VAR lcRegion     AS CHAR NO-UNDO.
DEF VAR llOk         AS LOG  NO-UNDO.
DEF VAR llUpdateCust AS LOG  NO-UNDO.
DEF VAR lcMemo       AS CHAR NO-UNDO.

DEF BUFFER bOrderCustomer FOR OrderCustomer.
DEF BUFFER bMobSub FOR MobSub.

FIND FIRST Order WHERE
           Order.Brand   = gcBrand AND
           Order.OrderId = iiOrderId 
EXCLUSIVE-LOCK NO-ERROR NO-WAIT.

IF locked(Order) THEN DO:
   if ilDisp THEN MESSAGE "Order " iiOrderId "is locked" view-as alert-box.
   RETURN "error".
END.

ASSIGN llCreateCust = FALSE
       liOldCustNum = 0
       llUpdateCust = FALSE.

/* is there a need to create customer */
IF iiRole = 2 AND Order.InvCustRole NE 2 THEN RETURN.
IF iiRole = 3 AND Order.UserRole NE 3    THEN RETURN.

FIND FIRST OrderCustomer NO-LOCK WHERE
           OrderCustomer.Brand   = gcBrand   AND
           OrderCustomer.OrderID = iiOrderID AND
           OrderCustomer.RowType = iiRole NO-ERROR.
IF NOT AVAILABLE OrderCustomer THEN DO:
   RETURN "ERROR: Customer data is not available for role " + STRING(iiRole).
END.

lcRegion = OrderCustomer.Region.

IF (LOOKUP(Order.OrderChannel,"Yoigo,Pre-act") > 0 OR
   Order.OrderType EQ {&ORDER_TYPE_RENEWAL} OR
   Order.OrderType EQ {&ORDER_TYPE_STC}) AND 
   OrderCustomer.CustNum > 0 
THEN liOldCustNum = OrderCustomer.CustNum.

ELSE IF iiRole = 1 THEN DO:

   IF OrderCustomer.CustNum > 0 AND 
     (Order.MNPStatus > 0 AND OrderCustomer.PersonId EQ "NEW") THEN
      liOldCustNum = OrderCustomer.CustNum.
   ELSE 
   run searchcust (INPUT  "ORGID|" +
                          /* if agrcust=invcust=user then show only those
                             that are invcusts to themselves */
                          (IF Order.InvCustRole = 1 AND
                              Order.UserRole = 1
                           THEN "SELF"
                           ELSE "") +
                          "|" + OrderCustomer.CustIDType, 
                   INPUT  OrderCustomer.CustID, 
                   INPUT  ilDisp,
                   OUTPUT lioldcustnum).  

END.

ELSE IF iiRole = 2 THEN DO:

   FIND FIRST bOrderCustomer NO-LOCK WHERE
              bOrderCustomer.Brand   = gcBrand   AND
              bOrderCustomer.OrderID = iiOrderID AND
              bOrderCustomer.RowType = 1 NO-ERROR.
 
   IF AVAILABLE bOrderCustomer THEN DO:

      run searchcust (INPUT  "INVCUST" + 
                          /* if user=agrcust then show only that customer's
                             invcust */
                          (IF Order.UserRole = 1
                           THEN "|" + STRING(bOrderCustomer.CustNum)
                           /* if user=invcust then show only customers that
                              are invcusts to themselves */
                           ELSE IF Order.UserRole = 2
                                THEN "|SELF"
                                /* otherwise show all under this agrcust */
                                ELSE ""),
                      INPUT  STRING(bOrderCustomer.CustNum),
                      INPUT  ilDisp,
                      OUTPUT lioldcustnum).
      
      lcRegion = bOrderCustomer.Region.
   END.
END.

ELSE IF iiRole = 3 THEN  DO:

   FIND FIRST bOrderCustomer NO-LOCK WHERE
              bOrderCustomer.Brand   = gcBrand   AND
              bOrderCustomer.OrderID = iiOrderID AND
              bOrderCustomer.RowType = Order.InvCustRole NO-ERROR.
   
   IF AVAILABLE bOrderCustomer THEN DO:

      run searchcust (INPUT  "USERCUST",
                      INPUT  STRING(bOrderCustomer.CustNum),
                      INPUT  ilDisp,
                      OUTPUT lioldcustnum).
      lcRegion = bOrderCustomer.Region.
   END.
END.

IF liOldCustnum = 0 THEN DO:
   
   /* get default invgroup and default customer through that */
   lcInvGroup = fDefInvGroup(lcRegion).
   
   new-custnum = fCParamI("DefCust" + lcInvGroup).
   IF new-Custnum = 0 OR new-custnum = ?
   THEN new-custnum = 300. 

   RUN copymobcu(INPUT-OUTPUT new-CustNum, INPUT FALSE).

   llOk = fmakeCustomer(Order.OrderID,
                        iiRole,    
                        TRUE,
                        INT(new-custnum),
                        FALSE).

   IF NOT llOk THEN DO:
      /* agr.cust is mandatory */
      IF iiRole = 1 THEN 
        UNDO, RETURN "ERROR:Customer data (" + STRING(iiRole) + 
               ") was not retrieved".
      /* separate user data is not mandatory */
      ELSE UNDO, RETURN "".
   END.

   IF iiRole = 1 AND OrderCustomer.SubQty > 0 THEN DO:
      
      FIND FIRST Limit WHERE
         Limit.CustNum = new-custnum AND
         Limit.LimitType = {&LIMIT_TYPE_SUBQTY} AND
         Limit.ToDate >= TODAY EXCLUSIVE-LOCK NO-ERROR.
     
      IF NOT AVAIL Limit THEN DO:
         CREATE Limit.
         ASSIGN
            Limit.CustNum = new-custnum
            Limit.LimitType = {&LIMIT_TYPE_SUBQTY}
            Limit.ValueType = 1
            Limit.FromDate  = TODAY
            Limit.ToDate    = 12/31/2049
            Limit.DefValue  = FALSE.
      END.
      
      Limit.LimitAmt  = OrderCustomer.SubQty.

      RELEASE Limit.

   END.

   ASSIGN OICustNum = new-custnum.

END.

ELSE DO:

   IF llDoEvent THEN DO:
      DEFINE VARIABLE lhCustomer AS HANDLE NO-UNDO.
      lhCustomer = BUFFER Customer:HANDLE.
      RUN StarEventInitialize(lhCustomer).

   END.
   
   oiCustnum = liOldCustnum.

   FIND Customer WHERE
        Customer.CustNum = oiCustNum EXCLUSIVE-LOCK NO-ERROR.
   IF NOT AVAILABLE Customer THEN
      RETURN "ERROR:Customer not found".

   FIND FIRST OrderCustomer NO-LOCK WHERE
              OrderCustomer.Brand   = gcBrand   AND
              OrderCustomer.OrderID = iiOrderID AND
              OrderCustomer.RowType = iiRole  NO-ERROR.

   IF llDoEvent THEN RUN StarEventSetOldBuffer ( lhCustomer ).
      
   /* YDR-1207, Always update marketing fields */
   ASSIGN
      Customer.DirMarkSMS   = OrderCustomer.OperSMSMarketing
      Customer.DirMarkEmail = OrderCustomer.OperEMailMarketing
      Customer.DirMarkPOST  = OrderCustomer.OperPostMarketing
      Customer.OutMarkSMS   = OrderCustomer.OutSMSMarketing
      Customer.OutMarkEmail = OrderCustomer.OutEMailMarketing
      Customer.OutMarkPOST  = OrderCustomer.OutPostMarketing.
      
   /* Renove order handling */
   IF Order.OrderType = {&ORDER_TYPE_RENEWAL} THEN DO:

      IF OrderCustomer.DataChecked = TRUE THEN
            fmakeCustomer(Order.OrderID,
                          iiRole,
                          FALSE,
                          oiCustnum,
                          FALSE).

      /* Update Email and Delivery type for all type of renewal orders */
      IF AVAIL OrderCustomer THEN
         fUpdEmailDelType(Order.OrderId).
   END.
   ELSE IF Order.OrderType EQ {&ORDER_TYPE_STC} THEN DO:
      /* bank account is changed with a separate request from stc process */
      ASSIGN
         Customer.SMSNumber   = OrderCustomer.MobileNumber.
      fUpdateEmail(Order.OrderId).
   END.

   /* DCH NEW/MNP */
   ELSE DO:

      FIND FIRST OrderCustomer EXCLUSIVE-LOCK WHERE
                 OrderCustomer.Brand   = gcBrand   AND
                 OrderCustomer.OrderID = iiOrderID AND
                 OrderCustomer.RowType = iiRole  NO-ERROR.

      IF AVAILABLE OrderCustomer THEN DO:

         IF llDoEvent THEN DO:
            DEFINE VARIABLE lhOrderCustomer AS HANDLE NO-UNDO.
            lhOrderCustomer = BUFFER OrderCustomer:HANDLE.
            RUN StarEventInitialize(lhOrderCustomer).
            IF llDoEvent THEN RUN StarEventSetOldBuffer ( lhOrderCustomer ).
         END.

         ASSIGN
            OrderCustomer.CustNum = oiCustNum
            OrderCustomer.PersonID = "OLD" WHEN OrderCustomer.PersonID EQ "".

         IF llDoEvent THEN RUN StarEventMakeModifyEvent ( lhOrderCustomer ).

         FIND FIRST MobSub NO-LOCK WHERE
                    MobSub.Brand   = gcBrand AND
                    MobSub.MsSeq   = Order.MsSeq AND
                    MobSub.CustNum = Customer.CustNum NO-ERROR.

         IF AVAILABLE MobSub THEN DO:
            IF MobSub.PayType = FALSE AND
               NOT CAN-FIND(FIRST bMobSub WHERE
                                  bMobSub.Brand     = gcBrand AND
                                  bMobSub.MsSeq    <> MobSub.MsSeq AND
                                  bMobSub.CustNum   = Customer.CustNum AND
                                  bMobSub.PayType   = FALSE) THEN
               llUpdateCust = TRUE.
         END.
         ELSE DO:
            IF Order.PayType = FALSE AND
               NOT CAN-FIND(FIRST bMobSub WHERE
                                  bMobSub.Brand     = gcBrand AND
                                  bMobSub.MsSeq    <> Order.MsSeq AND
                                  bMobSub.CustNum   = Customer.CustNum AND
                                  bMobSub.PayType   = FALSE) THEN
               llUpdateCust = TRUE.
         END.

         IF llUpdateCust THEN DO:
            ASSIGN
               Customer.HonTitle        = OrderCustomer.CustTitle
               Customer.FirstName       = TRIM(OrderCustomer.FirstName)
               Customer.CustName        = TRIM(OrderCustomer.Surname1)
               Customer.SurName2        = TRIM(OrderCustomer.SurName2)
               Customer.Nationality     = OrderCustomer.Nationality
               Customer.Language        = INTEGER(OrderCustomer.Language)
               Customer.BirthDay        = OrderCustomer.BirthDay
               Customer.Phone           = OrderCustomer.FixedNumber
               Customer.SMSNumber       = OrderCustomer.MobileNumber
               Customer.BankAcc         = OrderCustomer.BankCode
               Customer.DirMarkSMS      = OrderCustomer.OperSMSMarketing
               Customer.Address         = OrderCustomer.Address
               Customer.ZipCode         = OrderCustomer.ZipCode
               Customer.PostOffice      = OrderCustomer.PostOffice
               Customer.Region          = OrderCustomer.Region
               Customer.Country         = OrderCustomer.Country
               Customer.DirMarkEmail    = OrderCustomer.OperEMailMarketing
               Customer.DirMarkPOST     = OrderCustomer.OperPostMarketing
               Customer.OutMarkSMS      = OrderCustomer.OutSMSMarketing
               Customer.OutMarkEmail    = OrderCustomer.OutEMailMarketing
               Customer.OutMarkPOST     = OrderCustomer.OutPostMarketing
               Customer.OutMarkBank     = OrderCustomer.OutBankMarketing
               Customer.CompanyName     = TRIM(OrderCustomer.Company) WHEN
                                          TRIM(OrderCustomer.Company) > ""
               Customer.FoundationDate  = OrderCustomer.FoundationDate WHEN
                                          OrderCustomer.CustIdType = "CIF"
               Customer.Profession      = TRIM(OrderCustomer.Profession) WHEN
                                          TRIM(OrderCustomer.Profession) > "".

            IF iiRole = 1 AND OrderCustomer.CustIdType = "CIF" AND
               Customer.CustIdType = "CIF" THEN DO:
               IF OrderCustomer.Rowtype = {&ORDERCUSTOMER_ROWTYPE_AGREEMENT} THEN ASSIGN
                  Customer.AuthCustId      = Order.OrdererID
                  Customer.AuthCustIdType  = Order.OrdererIDType.
               ELSE DO:
                  FIND CustContact WHERE
                       CustContact.Brand = gcBrand AND
                       CustContact.Custnum = Customer.CustNum AND
                       CustContact.CustType = {&CUSTCONTACT_CONTACT} EXCLUSIVE-LOCK NO-ERROR.
                  IF AVAILABLE CustContact THEN DO:
                     ASSIGN
                        CustContact.HonTitle       = OrderCustomer.CustTitle
                        CustContact.FirstName      = OrderCustomer.FirstName
                        CustContact.CustName       = OrderCustomer.Surname1
                        CustContact.Surname2       = OrderCustomer.Surname2
                        CustContact.Nationality    = OrderCustomer.Nationality
                        CustContact.Language       = INT(OrderCustomer.Language)
                        CustContact.SMSNumber      = OrderCustomer.MobileNumber
                        CustContact.Email          = OrderCustomer.Email
                        CustContact.CustIdType     = OrderCustomer.CustIdType
                        CustContact.OrgId          = OrderCustomer.Custid.
                  END.
               END.
            END.

            fUpdEmailDelType(Order.OrderId).

            /* check if bank data is now available */
            IF iiRole = 1 OR iiRole = 2 THEN DO:
               IF AVAILABLE Customer AND Customer.BankAcc = "" AND
                  Customer.OrgID = OrderCustomer.CustID
               THEN DO:

                  FIND Current Customer EXCLUSIVE-LOCK.
                  Customer.BankAcc = OrderCustomer.BankCode.

                  IF OrderCustomer.PersonID = "NEW" AND
                     Order.MNPStatus > 0 THEN
                     ASSIGN
                        Customer.OrgId = OrderCustomer.CustId
                        Customer.CustIdType = OrderCustomer.CustIdType.
               END.
            END.

         END. /* IF llUpdateCust THEN DO: */
      END.
   END.

   lcMemo = "Order" + CHR(255) +
            STRING(Customer.CustNum) + CHR(255) +
            STRING(Order.OrderId) + CHR(255) +
            Order.Salesman.

   IF llDoEvent THEN RUN StarEventMakeModifyEventWithMemo(
                           lhCustomer,
                           katun,
                           lcMemo).
   
   RELEASE Customer.

END.

IF iiRole = 1 THEN Order.CustNum = oiCustNum.

return "ok".

FINALLY:
   IF llDoEvent THEN fCleanEventObjects().
END.
