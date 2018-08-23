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
{Syst/commali.i} 
{Func/cparam2.i}
{Syst/eventval.i}
{Func/forderstamp.i}
{Syst/tmsconst.i}
{Func/order.i}
{Func/fcustdata.i}

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER Syst.Var:katun
   {Func/lib/eventlog.i}
END.

DEF INPUT  PARAMETER  iiOrderId  LIKE Order.OrderId.
DEF INPUT  PARAMETER  iiRole     AS   INT.
DEF INPUT  PARAMETER  ilDisp     AS   LOG.
DEF INPUT  PARAMETER  ilUpdateExisting AS LOG.
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
DEF VAR lcMemo       AS CHAR NO-UNDO.
DEF VAR llProToNonPro AS LOG NO-UNDO. 
DEF VAR llPayType AS LOG NO-UNDO. 

DEF BUFFER bOrderCustomer FOR OrderCustomer.
DEF BUFFER bMobSub FOR MobSub.

FIND FIRST Order WHERE
           Order.Brand   = Syst.Var:gcBrand AND
           Order.OrderId = iiOrderId 
EXCLUSIVE-LOCK NO-ERROR NO-WAIT.

IF locked(Order) THEN DO:
   if ilDisp THEN MESSAGE "Order " iiOrderId "is locked" view-as alert-box.
   RETURN "error".
END.

ASSIGN llCreateCust = FALSE
       liOldCustNum = 0.

/* is there a need to create customer */
IF iiRole = 2 AND Order.InvCustRole NE 2 THEN RETURN.
IF iiRole = 3 AND Order.UserRole NE 3    THEN RETURN.

FIND FIRST OrderCustomer NO-LOCK WHERE
           OrderCustomer.Brand   = Syst.Var:gcBrand   AND
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
   RUN Mc/searchcust.p (INPUT  "ORGID|" +
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
              bOrderCustomer.Brand   = Syst.Var:gcBrand   AND
              bOrderCustomer.OrderID = iiOrderID AND
              bOrderCustomer.RowType = 1 NO-ERROR.
 
   IF AVAILABLE bOrderCustomer THEN DO:

      RUN Mc/searchcust.p (INPUT  "INVCUST" + 
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
              bOrderCustomer.Brand   = Syst.Var:gcBrand   AND
              bOrderCustomer.OrderID = iiOrderID AND
              bOrderCustomer.RowType = Order.InvCustRole NO-ERROR.
   
   IF AVAILABLE bOrderCustomer THEN DO:

      RUN Mc/searchcust.p (INPUT  "USERCUST",
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

   RUN Mm/copymobcu.p(INPUT-OUTPUT new-CustNum, INPUT FALSE).

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

   oiCustnum = liOldCustnum.
   IF Order.OrderType EQ {&ORDER_TYPE_NEW} OR 
      Order.OrderType EQ {&ORDER_TYPE_MNP} THEN DO:

      FIND FIRST OrderCustomer EXCLUSIVE-LOCK WHERE
                 OrderCustomer.Brand   = Syst.Var:gcBrand   AND
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
            IF OrderCustomer.RowType = 1 THEN Order.CustNum = oiCustNum.

         IF llDoEvent THEN RUN StarEventMakeModifyEvent ( lhOrderCustomer ).
      END.
   END.

   IF ilUpdateExisting EQ FALSE THEN RETURN "not updated existing customer".
   
   IF llDoEvent THEN DO:
      DEFINE VARIABLE lhCustomer AS HANDLE NO-UNDO.
      lhCustomer = BUFFER Customer:HANDLE.
      RUN StarEventInitialize(lhCustomer).

   END.

   FIND Customer WHERE
        Customer.CustNum = oiCustNum EXCLUSIVE-LOCK NO-ERROR.
   IF NOT AVAILABLE Customer THEN
      RETURN "ERROR:Customer not found".

   FIND FIRST OrderCustomer NO-LOCK WHERE
              OrderCustomer.Brand   = Syst.Var:gcBrand   AND
              OrderCustomer.OrderID = iiOrderID AND
              OrderCustomer.RowType = iiRole  NO-ERROR.

   IF llDoEvent THEN RUN StarEventSetOldBuffer ( lhCustomer ).
      
   /* YDR-1207, Always update marketing fields */
   ASSIGN
      Customer.DirMarkSMS        = OrderCustomer.OperSMSMarketing
      Customer.DirMarkEmail      = OrderCustomer.OperEMailMarketing
      Customer.DirMarkPOST       = OrderCustomer.OperPostMarketing
      Customer.OutMarkSMS        = OrderCustomer.OutSMSMarketing
      Customer.OutMarkEmail      = OrderCustomer.OutEMailMarketing
      Customer.OutMarkPOST       = OrderCustomer.OutPostMarketing
      Customer.DontSharePersData = OrderCustomer.DontSharePersData.

   llProToNonPro = (fIsPro(Customer.Category) EQ TRUE AND
                    fIsPro(OrderCustomer.Category) EQ FALSE).
      
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
      DO:
         fUpdEmailDelType(Order.OrderId).

         IF Customer.Category <> OrderCustomer.Category AND
            NOT llProToNonPro THEN
            ASSIGN Customer.Category = OrderCustomer.Category.
      END.   
   END.
   ELSE IF Order.OrderType EQ {&ORDER_TYPE_STC} THEN DO:
      /* bank account is changed with a separate request from stc process */
      ASSIGN Customer.SMSNumber   = OrderCustomer.MobileNumber.
      fUpdateEmail(Order.OrderId).
      /* YPRO migrate YPRO-92 category */
      IF Ordercustomer.Category NE Customer.Category AND
         NOT llProToNonPro THEN
         Customer.Category = Ordercustomer.Category.
   END.

   /* DCH NEW/MNP */
   ELSE DO:
         FIND FIRST MobSub NO-LOCK WHERE
                    MobSub.Brand   = Syst.Var:gcBrand AND
                    MobSub.MsSeq   = Order.MsSeq AND
                    MobSub.CustNum = Customer.CustNum NO-ERROR.

         IF AVAILABLE MobSub THEN
            llPayType = MobSub.PayType.
         ELSE llPayType = Order.PayType.
            
         IF llPayType EQ FALSE AND
            NOT CAN-FIND(FIRST bMobSub WHERE
                               bMobSub.Brand     = Syst.Var:gcBrand          AND
                               bMobSub.MsSeq    <> Order.MsSeq     AND
                               bMobSub.CustNum   = Customer.CustNum AND
                               bMobSub.PayType   = FALSE)           THEN DO:
            ASSIGN
               Customer.HonTitle        = OrderCustomer.CustTitle
               Customer.FirstName       = TRIM(OrderCustomer.FirstName)
               Customer.CustName        = TRIM(OrderCustomer.Surname1)
               Customer.SurName2        = TRIM(OrderCustomer.SurName2)
               Customer.SearchName      = SUBSTRING(Customer.CustName +
                                                    Customer.FirstName,1,8)
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
               Customer.InvGroup        = fDefInvGroup(OrderCustomer.Region)
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
                                          TRIM(OrderCustomer.Profession) > ""
               Customer.AuthCustId      = OrderCustomer.AuthCustId
                  WHEN OrderCustomer.Rowtype = {&ORDERCUSTOMER_ROWTYPE_AGREEMENT}
               Customer.AuthCustIdType  = OrderCustomer.AuthCustIdType
                  WHEN OrderCustomer.Rowtype = {&ORDERCUSTOMER_ROWTYPE_AGREEMENT}
               Customer.Category   = OrderCustomer.Category WHEN
                                     OrderCustomer.CustID EQ Customer.OrgID AND
                                     OrderCustomer.CustIDType EQ Customer.CustIDType.

            /* check if bank data is now available */
            IF iiRole = 1 OR iiRole = 2 THEN DO:
               IF AVAILABLE Customer AND Customer.BankAcc = "" AND
                  Customer.OrgID = OrderCustomer.CustID AND
                  Customer.CustidType = OrderCustomer.CustIDType
               THEN DO:

                  FIND Current Customer EXCLUSIVE-LOCK.
                  Customer.BankAcc = OrderCustomer.BankCode.

                  IF OrderCustomer.PersonID = "NEW" AND
                     Order.MNPStatus > 0 THEN
                     ASSIGN
                        Customer.OrgId = OrderCustomer.CustId
                        Customer.CustIdType = OrderCustomer.CustIdType
                        Customer.Category = OrderCustomer.Category.
               END.
            END.

         END.
         ELSE IF Customer.Category <> OrderCustomer.Category AND
             NOT llProToNonPro THEN
             ASSIGN Customer.Category = OrderCustomer.Category.

         IF NOT OrderCustomer.Pro THEN /* Order is with non-pro, so closing all ACC requests for pro */
            fClosePendingACC("Pro", OrderCustomer.CustIdType, OrderCustomer.CustId, Order.OrderId).
         ELSE                          /* Order is with non-pro, so closing all ACC requests for pro */   
            fClosePendingACC("Non-pro", OrderCustomer.CustIdType, OrderCustomer.CustId, Order.OrderId).

         fUpdEmailDelType(Order.OrderId).

   END.

   lcMemo = "Order" + CHR(255) +
            STRING(Customer.CustNum) + CHR(255) +
            STRING(Order.OrderId) + CHR(255) +
            Order.Salesman + CHR(255) +
            Order.OrderChannel.

   IF llDoEvent THEN RUN StarEventMakeModifyEventWithMemo(
                           lhCustomer,
                           Syst.Var:katun,
                           lcMemo).
   
   RELEASE Customer.

END.

IF iiRole = 1 THEN Order.CustNum = oiCustNum.

return "ok".

FINALLY:
   IF llDoEvent THEN fCleanEventObjects().
END.
