/* ----------------------------------------------------------------------
  MODULE .......: aftersalesreq.p 
  TASK .........: Handles "After Sales Order" request (type 46)
  APPLICATION ..: TMS
  AUTHOR .......: anttis 
  CREATED ......: 12.09.08
  Version ......: xfera
----------------------------------------------------------------------- */

{commali.i}
{cparam2.i}
{fmakemsreq.i}
{forderstamp.i}
{orderfunc.i}
{fsubsterminal.i}
{tmsconst.i}
{offer.i}
{fcpfat.i}
{tmsconst.i}

DEFINE INPUT PARAMETER iiMSrequest AS INT NO-UNDO.

DEFINE VARIABLE lcCharValue     AS CHARACTER NO-UNDO. 
DEFINE VARIABLE ldaDate         AS DATE      NO-UNDO. 
DEFINE VARIABLE liTime          AS INTEGER   NO-UNDO. 
DEFINE VARIABLE iRenoveSMSLang  AS INTEGER   NO-UNDO.
DEFINE VARIABLE lcRenoveSMSText AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcEmailItems    AS CHARACTER NO-UNDO. 
DEFINE VARIABLE ldEmailInvTot   AS DECIMAL   NO-UNDO. 
DEFINE VARIABLE lcEmailError    AS CHARACTER NO-UNDO. 
DEFINE VARIABLE oiCustomer      AS INTEGER   NO-UNDO. 
DEFINE VARIABLE ldAmount        AS DECIMAL   NO-UNDO.
DEFINE VARIABLE ocError         AS CHARACTER NO-UNDO.
DEFINE VARIABLE ldeTermTS       AS DECIMAL   NO-UNDO.
DEFINE VARIABLE lcFatGroup      AS CHARACTER NO-UNDO.
DEFINE VARIABLE ldeMonthlyFee   AS DECIMAL   NO-UNDO. 
DEFINE VARIABLE liMonths        AS INTEGER   NO-UNDO. 
DEFINE VARIABLE lcSMSType       AS CHARACTER NO-UNDO.
DEFINE VARIABLE ldLastMonthDate AS DATE      NO-UNDO. 
DEFINE VARIABLE liLastPeriod    AS INTEGER   NO-UNDO.
DEFINE VARIABLE ldeTimeStamp    AS DECIMAL   NO-UNDO. 
DEFINE VARIABLE ldeFinalFee     AS DECIMAL   NO-UNDO.

/* msrequest parameters */
DEFINE VARIABLE lcError AS CHARACTER NO-UNDO. 
DEFINE VARIABLE liRequest AS INTEGER NO-UNDO.

DEF BUFFER bACCRequest FOR MsRequest.
DEF BUFFER bMobSub FOR MobSub.

FIND MSRequest WHERE 
     MSRequest.MSRequest = iiMSRequest
NO-LOCK NO-ERROR.

IF NOT AVAILABLE MsRequest OR 
                 MsRequest.ReqType NE 46 THEN RETURN "ERROR".

/* request is under work */
IF NOT fReqStatus(1,"") THEN RETURN "ERROR".

FIND Order WHERE
     Order.Brand   = gcBrand AND 
     Order.OrderId = MsRequest.ReqIParam1 EXCLUSIVE-LOCK NO-ERROR.
   
IF NOT AVAILABLE Order THEN DO:
   fReqError("Order not found").
   RETURN.
END.

IF Order.StatusCode NE "12" THEN DO:
   fReqError("Order is in wrong status: " + Order.StatusCode).
   RETURN.
END.

FIND Mobsub WHERE
     Mobsub.MsSeq = MsRequest.MsSeq NO-LOCK NO-ERROR.

IF NOT AVAILABLE Mobsub THEN DO:
   fReqError("Subscription not found").
   RETURN.
END.

/* acc prevents new orders */
IF CAN-FIND(FIRST bACCRequest WHERE
                  bACCRequest.MsSeq   = MobSub.MsSeq AND
                  bACCRequest.ReqType = 10 AND
                  LOOKUP(STRING(bACCRequest.ReqStat),"2,4,9") = 0)
THEN DO:
   fReqError("Ongoing ACC").
   RETURN.
END.

/* Send order confirmation email */
RUN prinoconf.p (Order.OrderID).

IF RETURN-VALUE BEGINS "ERROR" THEN DO:
   DYNAMIC-FUNCTION("fWriteMemo" IN ghFunc1,
                  "Order",
                  STRING(Order.OrderID),
                  0,
                  "Order Confirmation Failed",
                  RETURN-VALUE).
END.

FIND OrderCustomer OF Order WHERE 
     OrderCustomer.RowType = 1 NO-LOCK NO-ERROR.

/* Send order confirmation SMS (exclude pos orders) */
IF Order.SMSType = 1 THEN DO:
   
   IF AVAIL OrderCustomer THEN 
      iRenoveSMSLang = INTEGER(OrderCustomer.Language). 
   ELSE iRenoveSMSLang = 1.
   
   /* SMS for Renewal in POS. YOT-1565 */
   IF INDEX(Order.OrderChannel,"pos") = 0 THEN DO:
      
      IF fGetOfferDeferredPayment(Order.Offer,
                                  Order.CrStamp,
                                  OUTPUT ldeMonthlyFee,
                                  OUTPUT liMonths,
                                  OUTPUT ldeFinalFee) > 0
      THEN lcSMSType = "RenoveOrderInst".
      ELSE lcSMSType = "RenoveOrderConf".

      lcRenoveSMSText = fGetSMSTxt(
                          lcSMSType,
                          TODAY,
                          iRenoveSMSLang,
                          OUTPUT ldeTimeStamp).

      IF lcRenoveSMSText > "" THEN DO:                    
         lcRenoveSMSText = REPLACE(lcRenoveSMSText,"#CONTRACT_ID",STRING(Order.ContractId)).
         fMakeSchedSMS(Order.CustNum,
                       Order.CLI,
                       41,
                       lcRenoveSMSText,
                       fMakeTS()).
      END.
   END.
END.

fSplitTS(Order.CRStamp, OUTPUT ldaDate, OUTPUT liTime). 
ldeTermTS = fMake2Dt(ldaDate - 1, 0).

/* terminals, do this before periodical contract creation */
fCreateSubsTerminal(BUFFER Order).

/* fatime is granted in a campaign */
IF Order.FatAmount > 0 OR Order.FtGrp > "" THEN DO:

   IF Order.FtGrp > "" AND 
      CAN-FIND(FIRST FatGroup WHERE
                     FatGroup.Brand = gcBrand AND
                     FatGroup.FtGrp = Order.FtGrp)
   THEN lcFatGroup = Order.FtGrp.
   ELSE lcFatGroup = fCParamC("OrderCampaignFat").
    
   IF lcFatGroup = ? OR lcFatGroup = "" THEN 
      lcError = "FATime group for campaign not defined".
      
   ELSE RUN creafat (MobSub.CustNum,
                     MobSub.MsSeq,
                     lcFatGroup,
                     Order.FatAmount,
                     0,
                     ?,
                     YEAR(ldaDate) * 100 + MONTH(ldaDate),
                     999999,
                     OUTPUT lcError).

   /* write possible error to an order memo */
   IF lcError > "" THEN
      DYNAMIC-FUNCTION("fWriteMemo" IN ghFunc1,
                       "Order",
                       STRING(Order.OrderID),
                       MobSub.CustNum,
                       "FATIME CREATION FAILED",
                       lcError).
END. /* IF Order.FatAmount > 0 OR Order.FtGrp > "" THEN DO: */

/* terminate and activate periodical contracts */
RUN requestaction_exec.p (MsRequest.MsRequest,
                        MobSub.CLIType,
                        Order.OrderID,
                        Order.CrStamp,
                        ldeTermTS,
                        TRUE,                   /* create fees */
                        "7",                    /* req.source */
                        {&REQUEST_ACTIONLIST_ALL}).

/* initial topup, fatime, per.contracts from offer */
IF Order.Offer > "" THEN 
   RUN offeritem_exec.p (MobSub.MsSeq,
                       Order.OrderID,
                       MsRequest.MsRequest,
                       {&REQUEST_SOURCE_RENEWAL}).
 
/* per.contract and service package created with the order */
RUN orderaction_exec.p (MobSub.MsSeq,
                      Order.OrderID,
                      MsRequest.MsRequest,
                      {&REQUEST_SOURCE_RENEWAL}).

/* YOT-1626 - Close promotion fat */
IF Order.CLIType = "CONT5" THEN DO:
   ASSIGN ldLastMonthDate = (DATE(MONTH(ldaDate),1,YEAR(ldaDate)) - 1)
          liLastPeriod    = (YEAR(ldLastMonthDate) * 100 +
                             MONTH(ldLastMonthDate)).
   fCloseFat("BONO8CPFREE",
             MsRequest.MsSeq,
             liLastPeriod).
END. /* IF Order.CLIType = "CONT5" THEN DO: */

/* update customer data */
RUN createcustomer.p(INPUT Order.OrderId,1,FALSE,TRUE,output oiCustomer).

/* update corporate customer contact data */
IF OrderCustomer.CustID = "CIF" THEN DO:

   FOR EACH OrderCustomer NO-LOCK WHERE
            OrderCustomer.Brand   = gcBrand   AND
            OrderCustomer.OrderID = Order.OrderID AND
            OrderCustomer.RowType = 5:

      RUN createcustcontact.p(
          Order.OrderId,
          MsRequest.Custnum,
          OrderCustomer.RowType,
          OUTPUT lcError).

      /* write possible error to an order memo */
      IF lcError > "" THEN DO:
         DYNAMIC-FUNCTION("fWriteMemo" IN ghFunc1,
                          "Order",
                          STRING(Order.OrderID),
                          oiCustomer,
                          "CUSTOMER CONTACT CREATION FAILED",
                          lcError).
      END.
   END.
END.


/* ICC change required, if new ICC is specified */
IF Order.OrderType = 2 AND Order.ICC > "" AND
   NOT Order.OrderChannel BEGINS "Renewal_POS" THEN DO:
   ASSIGN liRequest = 0
          lcError   = ""
          katun     = Order.SalesMan WHEN Order.SalesMan > "".

   liRequest = fSubscriptionRequest
                   (INPUT  Order.MSSeq,
                    INPUT  Order.CLI,
                    INPUT  Order.CustNum,
                    INPUT  1,
                    INPUT  "",
                    INPUT  fMakeTS(),
                    INPUT  "CHANGEICC",
                    INPUT  Order.ICC,
                    INPUT  "", /*for old SIM*/ 
                    INPUT  "", /*for Reason info*/
                    INPUT  "", /*for ContractID*/
                    INPUT  FALSE,
                    INPUT  0.0,
                    INPUT  {&REQUEST_SOURCE_RENEWAL},
                    OUTPUT lcError).
   IF liRequest = 0 THEN
      DYNAMIC-FUNCTION("fWriteMemo" IN ghFunc1,
                       "Order",
                       STRING(Order.OrderID),
                       Order.CustNum,
                       "ICC change request creation failed",
                       lcError).
   ELSE DO:
      FIND MsRequest WHERE
           MsRequest.MsRequest = liRequest EXCLUSIVE-LOCK NO-ERROR.
      MsRequest.ReqSource = {&REQUEST_SOURCE_ICC_CHANGE_AUTO}.
      fReqStatus(19,"").

      DYNAMIC-FUNCTION("fWriteMemo" IN ghFunc1,
                       "MsRequest",
                       STRING(liRequest),
                       Order.CustNum,
                       "ICC TYPE CHANGE AUTO",
                       Order.OrderChannel).
   END. /* ELSE DO: */

   katun = "request".

END. /* IF Order.OrderType = 2 AND Order.ICC > "" AND */

/* skip dextra handling */

/* temp solution to pass direct channel orders 
IF Order.OrderChannel BEGINS "renewal_pos" THEN DO: */
   fSetOrderStatus(Order.OrderId,"6").
   /* Mark the timestamp as delivery */
   fMarkOrderStamp(Order.OrderId,"Delivery",0.0).
/* END.  */

RELEASE Order.

/* Find original buffer */
FIND MsRequest WHERE 
     MsRequest.MsRequest = iiMSrequest NO-LOCK NO-ERROR.

fReqStatus(2,""). /* request handled succesfully */
