/* ----------------------------------------------------------------------
  MODULE .......: acc_order.p
  TASK .........: Creates ACC request from ACC order
  APPLICATION ..: TMS
  AUTHOR .......: anttis
  CREATED ......: 19.04.18
  Version ......: Masmovil
----------------------------------------------------------------------- */
{Func/fmakemsreq.i}
{Func/cparam2.i}
{Syst/tmsconst.i}
{Mm/msagrcustchg.i}
{Func/fcustchangereq.i}
{Func/orderproduct.i}

DEF INPUT PARAM piOrderID AS INT NO-UNDO. 
DEF OUTPUT PARAM oiRequest AS INT NO-UNDO. 

DEF VAR lcOrigKatun AS CHAR NO-UNDO. 
DEF VAR lcError AS CHAR NO-UNDO. 
DEF VAR lcReqSource AS CHAR NO-UNDO. 
DEF VAR llProCust AS LOG NO-UNDO. 
DEF VAR llSelfEmployed AS LOG NO-UNDO. 
DEF VAR liSubLimit AS INT NO-UNDO. 
DEF VAR liSubs AS INT NO-UNDO. 
DEF VAR liActLimit AS INT NO-UNDO. 
DEF VAR liActs AS INT NO-UNDO. 
DEF VAR ldaAccDate AS DATE NO-UNDO. 

DEF VAR ldeChgStamp AS DEC NO-UNDO. 

DEF BUFFER bOriginalCustomer FOR Customer.

FIND Order NO-LOCK WHERE
     Order.Brand = Syst.Var:gcBrand AND
     Order.Orderid = piOrderID NO-ERROR.
IF NOT AVAIL Order THEN RETURN "ERROR:Order not found".

IF Order.OrderType NE {&ORDER_TYPE_ACC} THEN
   RETURN "ERROR:Incorrect order type".
         
FIND OrderCustomer NO-LOCK WHERE
     OrderCustomer.Brand = Order.Brand AND
     OrderCustomer.OrderID = Order.OrderID AND
     OrderCustomer.RowType = {&ORDERCUSTOMER_ROWTYPE_ACC} NO-ERROR.
IF NOT AVAIL OrderCustomer THEN RETURN "ERROR:OrderCustomer not found".

FIND bOriginalCustomer NO-LOCK WHERE
     bOriginalCustomer.Custnum = Order.Custnum NO-ERROR.
IF NOT AVAIL bOriginalCustomer THEN RETURN "ERROR:Old customer not found".

IF OrderCustomer.Custnum > 0 THEN DO:
   FIND Customer NO-LOCK WHERE
        Customer.Custnum = OrderCustomer.Custnum NO-ERROR.
   IF NOT AVAIL Customer THEN RETURN "ERROR:New customer not found".
END.

FIND OrderProduct NO-LOCK WHERE
     OrderProduct.OrderID = Order.OrderID AND
     OrderProduct.ActionType = "acc" NO-ERROR.
IF NOT AVAIL OrderProduct THEN RETURN "ERROR:Order product not found".

FIND FIRST MobSub NO-LOCK WHERE
           MobSub.MsSeq = Order.MsSeq NO-ERROR.
IF NOT AVAIL MobSub THEN
   RETURN "ERROR:Subscription is not active".

IF MobSub.Custnum NE Order.Custnum THEN
   RETURN SUBSTITUTE("ERROR:The subscription's (MsSeq = '&1') " +
                     "customer number '&2' doesn't match " +
                     "to the donor customer number '&3'",
                     MobSub.MsSeq,
                     MobSub.CustNum,
                     Order.CustNum).

lcOrigKatun = Syst.Var:katun.
Syst.Var:katun = "VISTA_" + Order.Salesman.

/*ACC is allowed for PRO-PRO and NON_PRO-NON_PRO*/
IF AVAIL Customer THEN
   lcError = Func.ValidateACC:mExistingCustomerACCCompability
                                    (bOriginalCustomer.Category,
                                     Customer.Category,
                                     Customer.CustNum,
                                     Customer.CustIdType,
                                     Customer.OrgId).


IF lcError > "" THEN RETURN lcError.                               

lcError = Func.ValidateACC:mPreCheckSubscriptionForACC(MobSub.MsSeq).
IF lcError > "" THEN lcError.

CASE Order.OrderChannel:
   WHEN "newton" THEN lcReqSource = {&REQUEST_SOURCE_NEWTON}.
   WHEN "retail_newton" THEN lcReqSource = {&REQUEST_SOURCE_RETAIL_NEWTON}.
   OTHERWISE  lcReqSource = {&REQUEST_SOURCE_MANUAL_TMS}.
END.

lcError = Func.ValidateACC:mCheckSubscriptionForACC(MobSub.MsSeq,
                                                    0,
                                                    Order.OrderID,
                                                    {&REQUEST_SOURCE_NEWTON}).
IF lcError > ""
THEN RETURN SUBSTRING(lcError,INDEX(lcError,"|") + 1).

IF AVAIL Customer THEN DO:
   lcError = Func.ValidateACC:mCheckTargetCustomerForACC(Customer.Custnum).
   IF lcError > "" THEN RETURN SUBSTRING(lcError,INDEX(lcError,"|") + 1).
END.
ELSE DO:
   lcError = Func.ValidateACC:mNewCustomerACCCompability(bOriginalCustomer.category,
                                                         OrderCustomer.CustId,
                                                         OrderCustomer.CustIdType).
   IF lcError > ""
   THEN RETURN lcError.
END.

ldaAccDate = fGetOPParamDate(OrderProduct.OrderProductID,
                             "execution-date").
IF ldaAccDate EQ ? OR
   ldaAccDate < TODAY THEN DO:
   IF MobSub.PayType EQ {&MOBSUB_PAYTYPE_PREPAID} THEN
      ldaAccDate = TODAY.
   ELSE IF MobSub.PayType EQ FALSE THEN DO:
      ldaAccDate = ADD-INTERVAL(TODAY,1,"months").
      ldaAccDate = DATE(MONTH(ldaAccDate),1,YEAR(ldaAccDate)).
   END.
   ldeChgStamp = Func.Common:mMake2DT(ldaAccDate,0).
END.
ELSE ldeChgStamp = Func.Common:mMake2DT(ldaAccDate,0).

oiRequest = fMSCustChangeRequest(
   MobSub.MsSeq,
   "agrcust",
   (IF AVAIL Customer THEN Customer.Custnum ELSE 0),
   MobSub.Custnum,
   SUBST("orderid:&1", Order.OrderID),
   ldeChgStamp,
   FALSE, /* create fees */
   0,
   TRUE,  /* send SMS */
   "",
   lcReqSource,
   0, /* orig. request */
   (IF Order.OrderChannel EQ {&DMS_VFR_REQUEST}
    THEN Order.ContractID ELSE ""),
   OUTPUT lcError).
  
IF oiRequest = 0 THEN
   RETURN "ACC request creation failed: " +  lcError.

RETURN "".

FINALLY:
   IF lcOrigKatun > "" THEN
      Syst.Var:katun = lcOrigKatun.
END.
