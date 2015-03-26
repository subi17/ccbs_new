/* ----------------------------------------------------------------------
  MODULE .......: createcustomer.p
  TASK .........: create new customer from order
  CREATED ......: JP
  CHANGED ......: 20.11.06/aam new db structure
                  12.04.07/aam update old customer's bank data 
  Version ......: yoigo
-------------------------------------------------------------------------- */
{commali.i} 
{timestamp.i}
{cparam.i2}
{eventval.i}
/* {fwebuser.i} */ 
{forderstamp.i}
{order.i}
{fcustdata.i}

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
DEF VAR llOk     AS LOG NO-UNDO.


DEF BUFFER bOrderCustomer FOR OrderCustomer.

FIND FIRST Order WHERE
           Order.Brand   = gcBrand AND
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
           OrderCustomer.Brand   = gcBrand   AND
           OrderCustomer.OrderID = iiOrderID AND
           OrderCustomer.RowType = iiRole NO-ERROR.
IF NOT AVAILABLE OrderCustomer THEN DO:
   RETURN "ERROR: Customer data is not available for role " + STRING(iiRole).
END.

lcRegion = OrderCustomer.Region.

IF (LOOKUP(Order.OrderChannel,"Yoigo,Pre-act") > 0 OR Order.OrderType EQ 2) AND 
   OrderCustomer.CustNum > 0 
THEN liOldCustNum = OrderCustomer.CustNum.

ELSE IF iiRole = 1 THEN DO:
/*
   IF OrderCustomer.CustNum > 0 THEN liOldCustNum = OrderCustomer.CustNum.
   ELSE  */
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
                        INT(new-custnum)).
   
   IF NOT llOk THEN DO:
      /* agr.cust is mandatory */
      IF iiRole = 1 THEN 
        RETURN "ERROR:Customer data (" + STRING(iiRole) + 
               ") was not retrieved".
      /* separate user data is not mandatory */
      ELSE RETURN "".
   END.

   IF iiRole = 1 AND OrderCustomer.SubQty > 0 THEN DO:
      
      FIND FIRST Limit WHERE
         Limit.CustNum = new-custnum AND
         Limit.LimitType = 2 AND
         Limit.ToDate >= TODAY EXCLUSIVE-LOCK NO-ERROR.
     
      IF NOT AVAIL Limit THEN DO:
         CREATE Limit.
         ASSIGN
            Limit.CustNum = new-custnum
            Limit.LimitType = 2
            Limit.LimitId   = 1
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
      &GLOBAL-DEFINE STAR_EVENT_USER katun

      {lib/eventlog.i}

      DEFINE VARIABLE lhCustomer AS HANDLE NO-UNDO.
      lhCustomer = BUFFER Customer:HANDLE.
      RUN StarEventInitialize(lhCustomer).

   END.
   
   oiCustnum = liOldCustnum.

   FIND Customer WHERE
        Customer.CustNum = oiCustNum NO-LOCK NO-ERROR.

   IF llDoEvent THEN RUN StarEventSetOldBuffer ( lhCustomer ).
      
   /* Remove order handling */
   IF Order.OrderType = 2 THEN DO:
      
      fmakeCustomer(Order.OrderID,
                    iiRole,    
                    FALSE,
                    oiCustnum).
   END.
   ELSE DO:
      FIND FIRST OrderCustomer EXCLUSIVE-LOCK WHERE
                 OrderCustomer.Brand   = gcBrand   AND
                 OrderCustomer.OrderID = iiOrderID AND
                 OrderCustomer.RowType = iiRole  NO-ERROR.

      IF AVAILABLE OrderCustomer THEN DO:
         
         ASSIGN
            OrderCustomer.CustNum = oiCustNum
            OrderCustomer.PersonID = "OLD" WHEN OrderCustomer.PersonID EQ "".
    
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
      END.
   END.
   
   IF llDoEvent THEN RUN StarEventMakeModifyEvent ( lhCustomer ).
   RELEASE Customer.

   fCleanEventObjects(). 

END.

IF iiRole = 1 THEN Order.CustNum = oiCustNum.
    
return "ok".

