 
 /*------------------------------------------------------------------------
    File        : Order
    Purpose     :
    Syntax      :
    Description :
    Author(s)   : tlamminmaki
    Created     : Mon Apr 23 12:30:10 EEST 2018
    Notes       :
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING profcgi.RPC.JSON.InternalError.
USING profcgi.RPC.JSON.ParamError.
USING Progress.Json.ObjectModel.JsonObject.
USING Progress.Json.ObjectModel.JsonArray.
BLOCK-LEVEL ON ERROR UNDO, THROW.

{Syst/tmsconst.i}

&GLOBAL-DEFINE nowTS (YEAR(TODAY) * 10000 + MONTH(TODAY) * 100 + DAY(TODAY) + TIME / 100000)

CLASS bss.cls.Order IMPLEMENTS bss.cls.IObjectStorage:
	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	
   DEFINE PUBLIC PROPERTY OrderId AS INTEGER NO-UNDO
      GET.
      SET.

   DEFINE PUBLIC PROPERTY aoCustomer AS CLASS bss.cls.Customer NO-UNDO
      GET.
      SET.
      
   DEFINE PUBLIC PROPERTY OrderPaymentData AS CLASS JsonObject NO-UNDO
       GET.
       SET.    
    
   DEFINE PUBLIC PROPERTY OrderInspectionData AS CLASS JsonObject NO-UNDO
       GET.
       SET.

   DEFINE PUBLIC PROPERTY ContractId AS CHARACTER NO-UNDO
      GET.
      SET.
      
   DEFINE PUBLIC PROPERTY loCustAcct AS CLASS JsonObject NO-UNDO
      GET.
      SET.
      
   DEFINE PUBLIC PROPERTY aoOrderInspection AS CLASS bss.cls.OrderInspection NO-UNDO 
      GET.
      SET.
      
   DEFINE PUBLIC PROPERTY aoOrderPayment AS CLASS bss.cls.OrderPayment NO-UNDO
      GET.
      SET.  
      
   DEFINE PUBLIC PROPERTY aoOrderAddress AS CLASS bss.cls.OrderAddress NO-UNDO 
      GET.
      SET.

   DEFINE PUBLIC PROPERTY Salesman AS CHARACTER NO-UNDO
      GET.
      SET.

   DEFINE PUBLIC PROPERTY OrderChannel AS CHARACTER NO-UNDO
      GET.
      SET.

   DEFINE PUBLIC PROPERTY CrStamp AS DECIMAL NO-UNDO
      GET.
      SET.

   DEFINE PUBLIC PROPERTY CLI AS CHARACTER NO-UNDO
      GET.
      SET.

   DEFINE PUBLIC PROPERTY MobSubCustNum AS INTEGER NO-UNDO
      GET.
      SET.
      
   DEFINE PUBLIC PROPERTY isACCOrder AS LOGICAL NO-UNDO
      GET.
      SET.
      
   DEFINE PUBLIC PROPERTY liCustNum AS INTEGER NO-UNDO
      GET.
      SET.
      
   DEFINE PUBLIC PROPERTY lcCLI AS CHARACTER NO-UNDO
      GET.
      SET(icCLI AS CHARACTER):
          
          ASSIGN THIS-OBJECT:lcCLI = icCLI.
          
      END SET.
      
   DEFINE PUBLIC PROPERTY ROIClose AS LOGICAL NO-UNDO
      GET.
      SET(INPUT llClose AS LOGICAL) :
          
          ASSIGN THIS-OBJECT:ROIClose   =   llClose .
          
      END SET.
   /* A value for this property is populated in OrderProduct class */
   DEFINE PUBLIC PROPERTY MsSeq AS INTEGER NO-UNDO
      GET.
      SET(iiMSSeq AS INTEGER):
         DEFINE BUFFER MobSub FOR MobSub.
         FIND MobSub NO-LOCK WHERE MobSub.MSSeq = iiMSSeq NO-ERROR.
         IF NOT AVAILABLE MobSub
         THEN DO:
            IF CAN-FIND(TermMobSub NO-LOCK WHERE TermMobSub.MSSeq = iiMSSeq)
            THEN UNDO, THROW NEW ParamError
                  (SUBSTITUTE("subscription_not_active|&1", iiMSSeq)).

            UNDO, THROW NEW ParamError
                  (SUBSTITUTE("subscription_not_found|&1", iiMSSeq)).
         END.
         ASSIGN
            THIS-OBJECT:MsSeq         = iiMSSeq
            THIS-OBJECT:CLI           = MobSub.CLI
            THIS-OBJECT:MobSubCustNum = MobSub.CustNum.
      END SET.

   DEFINE PUBLIC PROPERTY aoOrderProductFactory AS CLASS bss.cls.OrderProductFactory NO-UNDO
      GET.
      SET.
      
   DEFINE PUBLIC PROPERTY loAddrArray AS CLASS JsonArray NO-UNDO 
       GET.
       SET.
       
   DEFINE PUBLIC PROPERTY CustAcctNum AS INTEGER NO-UNDO
      GET.
      SET.
      
   DEFINE PUBLIC PROPERTY isPrePaidOrder AS LOGICAL NO-UNDO
      GET.
      SET.
      
   DEFINE PUBLIC PROPERTY OrderType AS INTEGER NO-UNDO
      GET.
      SET (iiType AS INTEGER):
          
          ASSIGN THIS-OBJECT:OrderType  = iiType.
          
      END SET.
      
    DEFINE PUBLIC PROPERTY isFixedOnly AS LOGICAL NO-UNDO
       GET.
       SET.
       
    DEFINE PUBLIC PROPERTY StatusCode AS CHARACTER NO-UNDO
       GET.
       SET(INPUT icStatusCode AS CHARACTER):
           
           ASSIGN THIS-OBJECT:StatusCode  =  icStatusCode.
           
       END SET.
       
    DEFINE PUBLIC PROPERTY lhOrderValidations AS HANDLE NO-UNDO 
       GET.
       SET.
       
    DEFINE PUBLIC PROPERTY MobileNumberType AS CHARACTER NO-UNDO
       GET.
       SET(INPUT icMType AS CHARACTER) :
           
           ASSIGN THIS-OBJECT:MobileNumberType  =  icMType.
           
       END SET.
       
    DEFINE PUBLIC PROPERTY isRenewalSTCOrder AS LOGICAL NO-UNDO
       GET.
       SET.
       
    DEFINE PUBLIC PROPERTY ROIOrderStatus AS CHARACTER NO-UNDO
       GET.
       SET.
       
    DEFINE PUBLIC PROPERTY CLIType AS CHARACTER NO-UNDO
       GET.
       SET.
       
    DEFINE PUBLIC PROPERTY lcICC AS CHARACTER NO-UNDO
       GET.
       SET.
       
    DEFINE PUBLIC PROPERTY FinalMsSeq AS INTEGER NO-UNDO
       GET.
       SET.
       
	CONSTRUCTOR PUBLIC Order():
	END CONSTRUCTOR.

	DESTRUCTOR PUBLIC Order():

      IF VALID-OBJECT(aoCustomer)
      THEN DELETE OBJECT aoCustomer.

      IF VALID-OBJECT(aoOrderProductFactory)
      THEN DELETE OBJECT aoOrderProductFactory.
      
        ASSIGN 
            OrderPaymentData    = ?
            OrderInspectionData = ?
            loCustAcct          = ?
            aoOrderInspection   = ?
            aoOrderPayment      = ?
            aoOrderAddress      = ?
            .

	END DESTRUCTOR.

	METHOD PUBLIC VOID mCreateData
	  (ioJsonObject AS CLASS JsonObject):

        DEFINE VARIABLE loAgrCust       AS CLASS     JsonObject           NO-UNDO.
        DEFINE VARIABLE ldaCreateDate   AS DATE      NO-UNDO.
        DEFINE VARIABLE liCount         AS INTEGER   NO-UNDO.
        DEFINE VARIABLE arrayAddrObject AS CLASS     JsonObject           NO-UNDO.
        DEFINE VARIABLE loAddrObject    AS CLASS     bss.cls.OrderAddress NO-UNDO.
        DEFINE VARIABLE lcMigrationErr  AS CHARACTER NO-UNDO.
              
      ASSIGN
         loAgrCust            =   ioJsonObject:GetJsonObject("agreement-customer")       WHEN ioJsonObject:Has("agreement-customer")
         loCustAcct           =   ioJsonObject:GetJsonObject("customer")                 WHEN ioJsonObject:Has("customer")
         OrderPaymentData     =   ioJsonObject:GetJsonObject("upfront_payment_method")   WHEN ioJsonObject:Has("upfront_payment_method")
         OrderInspectionData  =   ioJsonObject:GetJsonObject("inspection")               WHEN ioJsonObject:Has("inspection")
         aoCustomer           =   NEW bss.cls.Customer().
         
      IF VALID-OBJECT(loAgrCust) 
      THEN DO:          
          IF loAgrCust:Has("custnum")
          THEN aoCustomer:mFindData(loAgrCust:GetInteger("custnum")).
          ELSE aoCustomer:mCreateData(loAgrCust).
          
          ASSIGN THIS-OBJECT:isACCOrder = TRUE.
      END. 
      ELSE IF VALID-OBJECT(loCustAcct)
      THEN DO:
          
          IF loCustAcct:Has("custnum")
          THEN DO:
              
              THIS-OBJECT:liCustNum   =  loCustAcct:GetInteger("custnum").
              aoCustomer:mFindData(THIS-OBJECT:liCustNum).
              IF loCustAcct:Has("account_id")
              THEN DO:              
                  ASSIGN 
                      THIS-OBJECT:CustAcctNum = loCustAcct:GetInteger("account_id").
                  aoCustomer:mFindCustomer(THIS-OBJECT:CustAcctNum).                  
              END.              
          END.          
          ELSE aoCustomer:mCreateData(loCustAcct).
      END.      

      ASSIGN
         ldaCreateDate            = Func.Common:mGetDate(ioJsonObject:GetCharacter("created-at"))
         THIS-OBJECT:ContractId   = ioJsonObject:GetCharacter("contractid") 
         THIS-OBJECT:Salesman     = ioJsonObject:GetCharacter("salesman")
         THIS-OBJECT:OrderChannel = ioJsonObject:GetCharacter("channel")
         THIS-OBJECT:CrStamp      = YEAR(ldaCreateDate) * 10000 +
                                    MONTH(ldaCreateDate) * 100 +
                                    DAY(ldaCreateDate)
         aoCustomer:isProCustomer = ( INDEX(THIS-OBJECT:OrderChannel,"PRO") > 0 )     
         aoCustomer:OrderChannel  = THIS-OBJECT:OrderChannel                   
         .
      
      IF ioJsonObject:Has("items")
      THEN DO:
         aoOrderProductFactory = NEW bss.cls.OrderProductFactory().
         aoOrderProductFactory:mCreateData(ioJsonObject:GetJsonArray("items"), THIS-OBJECT).
      END.
      ELSE UNDO, THROW NEW ParamError("order_items_are_mandatory").
      
      RUN Mc/ordervalidations.p PERSISTENT SET lhOrderValidations.  /* This is for the complicated business logic */
      
      IF THIS-OBJECT:OrderChannel BEGINS "migration"
      AND THIS-OBJECT:isACCOrder  = FALSE
      THEN DO:
          ASSIGN lcMigrationErr =  DYNAMIC-FUNCTION("fMigrationCheckCustomer" IN THIS-OBJECT:lhOrderValidations , INPUT Syst.Var:gcBrand , INPUT aoCustomer:CustId).
          IF lcMigrationErr > ""
          THEN UNDO , THROW NEW ParamError("Migration data validation error:" + lcMigrationErr).
      END.
      
      IF VALID-OBJECT(OrderInspectionData)
      THEN DO:
          ASSIGN aoOrderInspection = NEW bss.cls.OrderInspection(THIS-OBJECT).
          aoOrderInspection:mCreateData(OrderInspectionData).
      END.
      
        
      IF VALID-OBJECT(OrderPaymentData)
      THEN DO:
          ASSIGN aoOrderPayment  = NEW bss.cls.OrderPayment(THIS-OBJECT).
          aoOrderPayment:mCreateData(OrderPaymentData).
      END.      
    
	END METHOD.

    METHOD PUBLIC VOID mStoreData():
        
        /* If there is a new customer data we need to create it first */
        IF aoCustomer:isNewCustomer THEN
        aoCustomer:mStoreData().
        
        CREATE Order.
        ASSIGN
            Order.Brand         = Syst.Var:gcBrand
            Order.OrderId       = NEXT-VALUE(OrderId)
            THIS-OBJECT:OrderId = Order.OrderId /* The OrderProducts needs this */            
            Order.Source        = "bssapi"
            Order.CustNum       = aoCustomer:CustNum
            Order.MsSeq         = (IF (THIS-OBJECT:isACCOrder OR THIS-OBJECT:isRenewalSTCOrder) THEN THIS-OBJECT:MsSeq ELSE NEXT-VALUE(MobSub))
            THIS-OBJECT:FinalMsSeq = Order.MsSeq
            Order.CLI           = (IF THIS-OBJECT:isACCOrder THEN THIS-OBJECT:CLI   ELSE THIS-OBJECT:lcCLI)
            Order.ContractID    = THIS-OBJECT:ContractId
            Order.SalesMan      = THIS-OBJECT:Salesman
            Order.OrderChannel  = THIS-OBJECT:OrderChannel
            Order.CrStamp       = THIS-OBJECT:CrStamp
            .
            
        CASE THIS-OBJECT:MobileNumberType:
            
            WHEN "new"       THEN Order.OrderType = {&ORDER_TYPE_NEW}.
            WHEN "mnp"       THEN Order.OrderType = {&ORDER_TYPE_MNP}.
            WHEN "renewal"   THEN Order.OrderType = {&ORDER_TYPE_RENEWAL}.
            WHEN "stc"       THEN Order.OrderType = {&ORDER_TYPE_STC}.
            WHEN "migration" THEN Order.OrderType = {&ORDER_TYPE_MNP}.
            
        END.
            
        IF VALID-OBJECT(OrderInspectionData)
        THEN aoOrderInspection:mStoreData().
            
        IF VALID-OBJECT(OrderPaymentData)
        THEN aoOrderPayment:mStoreData().
        
        aoOrderProductFactory:mStoreData(). 
        
        IF THIS-OBJECT:isACCOrder = FALSE 
        THEN DO:
            
            IF THIS-OBJECT:lcCLI > ""
            THEN DO:
                mCloseOfferedOrderWithSameNumber().
                mCheckDuplicateOrders().
            END. 
            
            IF THIS-OBJECT:MobileNumberType = {&TYPE_NEW} AND 
               THIS-OBJECT:ROIClose         = FALSE 
            THEN mMakeMSISDNHistory().
            
            IF THIS-OBJECT:ROIClose    = FALSE 
            THEN DO:
           
                IF THIS-OBJECT:StatusCode    <> {&ORDER_STATUS_IN_CONTROL} AND 
                   THIS-OBJECT:OrderChannel  BEGINS "RETENTION" OR 
                   (Func.ValidateOrder:mIsConvergentTariff(THIS-OBJECT:CLIType) AND THIS-OBJECT:MobileNumberType = "STC") AND 
                    Mnp.MNPOutGoing:mIsMNPOutOngoing(INPUT THIS-OBJECT:lcCLI)
                THEN DO:
                     
                     ASSIGN THIS-OBJECT:StatusCode  =  {&ORDER_STATUS_MNP_RETENTION}.
                     
                     IF THIS-OBJECT:OrderChannel = "retention_stc" THEN 
                     DO:
                         FIND FIRST MsRequest WHERE
                                    MsRequest.MsSeq = Order.MsSeq AND
                                    MsRequest.ReqType = 0 AND
                             LOOKUP(STRING(MsRequest.ReqStatus),"2,4,9") = 0
                             EXCLUSIVE-LOCK NO-ERROR.
                         IF AVAIL MsRequest THEN
                             MsRequest.ReqIParam2 = THIS-OBJECT:OrderId.
                         ELSE 
                         DO:
                             THIS-OBJECT:StatusCode = {&ORDER_STATUS_IN_CONTROL}.
                             Func.Common:mWriteMemoWithType("Order",
                                                            STRING(THIS-OBJECT:OrderId),
                                                            0,
                                                            "STC request was not found",
                                                            "",
                                                            "",
                                                            "BSSAPI").
                         END. /* ELSE DO: */
                     END. /* IF Order.OrderChannel = "retention_stc" THEN DO: */
                END. /* IF THIS-OBJECT:StatusCode    <> {&ORDER_STATUS_IN_CONTROL} AND  */
                ELSE IF THIS-OBJECT:StatusCode <> {&ORDER_STATUS_IN_CONTROL}
                THEN DO:
                    
                    /*TO-DO PC: PLEASE IMPLEMENT BELOW LOGIC FROM WHOLE ORDER RPC*/
                    
                    /*IF pcNumberType EQ "renewal" THEN  ==> NOT IMPLEMENTED THIS LOGIC*/   
                    
                    IF THIS-OBJECT:aoCustomer:CustIdType = "CIF" THEN 
                        mCompanyCustomerStatus().
                    ELSE IF THIS-OBJECT:OrderChannel BEGINS "fusion" THEN
                        ASSIGN THIS-OBJECT:StatusCode = {&ORDER_STATUS_PENDING_FIXED_LINE}.
                    ELSE DO:
                        
                        IF THIS-OBJECT:MobileNumberType     = {&TYPE_MNP} OR 
                           THIS-OBJECT:MobileNumberType     = {&TYPE_MIGRATION}
                        THEN ASSIGN THIS-OBJECT:StatusCode  = {&ORDER_STATUS_MNP}.
                        ELSE ASSIGN THIS-OBJECT:StatusCode  = {&ORDER_STATUS_NEW}.
                        
                        /*TO-DO PC : STATUS Change based on the multi sim type and multi sim id*/
                        /*TO-DO PC : STATUS Change based on pending main line and porting date*/
                        
                    END. /* ELSE DO: */                    
                END. /* ELSE IF THIS-OBJECT:StatusCode <> {&ORDER_STATUS_IN_CONTROL} */                     
            END. /* IF THIS-OBJECT:ROIClose    = FALSE  */
            
            IF THIS-OBJECT:ROIOrderStatus > "" 
            THEN ASSIGN THIS-OBJECT:StatusCode = THIS-OBJECT:ROIOrderStatus.
                
            IF THIS-OBJECT:lcICC  GT ""   
            THEN mCreateChangeICCRequest().
                
        END. /*IF THIS-OBJECT:isACCOrder = FALSE */

        ASSIGN 
            Order.StatusCode    = THIS-OBJECT:StatusCode
            Order.Paytype       = (IF Func.ValidateOrder:mGetPayType(INPUT THIS-OBJECT:CLIType) = 2 THEN YES ELSE NO) .
        
    END METHOD.
    
    METHOD PUBLIC VOID mCloseOfferedOrderWithSameNumber():
        
        DEFINE BUFFER bfpcOrder FOR Order.
        
        IF THIS-OBJECT:OrderChannel BEGINS "RETENTION"
        THEN DO:
            
            FOR EACH bfpcOrder NO-LOCK WHERE
                     bfpcOrder.Brand      =     Syst.Var:gcBrand           AND
                     bfpcOrder.CLI        =     THIS-OBJECT:lcCLI          AND
                     bfpcOrder.StatusCode =     {&ORDER_STATUS_OFFER_SENT} AND /* shouldn't never get this value because of YDR-2575 */
                ROWID(bfpcOrder) <> ROWID(Order):

                RUN Mc/closeorder.p(bfpcOrder.OrderId, TRUE) NO-ERROR.

                IF RETURN-VALUE > ""
                THEN Func.Common:mWriteMemoWithType("Order",
                                                    STRING(THIS-OBJECT:OrderId),
                                                    0,
                                                    "Automatic order closing failed",
                                                    SUBST("Failed to close pending order. " + 
                                                    "Order ID: &1, Error: &2", 
                                                    bfpcOrder.orderid, RETURN-VALUE),
                                                    "",
                                                    "Newton RPC").
                                                        
            END. /* FOR EACH bfpcOrder */
        END. /* IF THIS-OBJECT:OrderChannel */
        
    END METHOD.

   METHOD PUBLIC JsonObject mResult():

      DEFINE VARIABLE loJsonObject AS CLASS JsonObject NO-UNDO.
      DEFINE VARIABLE loCustObject AS CLASS JsonObject NO-UNDO.
      DEFINE VARIABLE loAddrObject AS CLASS JsonObject NO-UNDO.
      DEFINE VARIABLE loAddrArray  AS CLASS JsonArray NO-UNDO.
      
      loJsonObject = NEW JsonObject().
      loJsonObject:Add("order-id", THIS-OBJECT:OrderId).
      
      IF aoCustomer:isNewCustomer 
      THEN DO:
          ASSIGN loCustObject = NEW JsonObject().
          loCustObject =  aoCustomer:mResult().
          loJsonObject:Add("customer" , loCustObject). 
      END.
       
      aoOrderProductFactory:mResult(loJsonObject).

      RETURN loJsonObject.

   END METHOD.
   
   METHOD PUBLIC VOID mCheckDuplicateOrders():
       
       IF Func.ValidateOrder:mCheckOtherOngoingOrders(INPUT THIS-OBJECT:lcCLI ,
                                                      INPUT THIS-OBJECT:MobileNumberType)
       THEN DO:
            
           ASSIGN THIS-OBJECT:StatusCode  =  {&ORDER_STATUS_IN_CONTROL}.
           
           Func.Common:mWriteMemoWithType("Order",
                                          STRING(Order.OrderId),
                                          0,
                                          "Order exists with same MSISDN",
                                          SUBST("Orderid: &1", Order.orderid),
                                          "",
                                          "BSSAPI"). 
                       
       END.                                               
       
   END METHOD.
   
   METHOD PUBLIC VOID mMakeMSISDNHistory():
       
       DEFINE BUFFER bfpcMSISDN FOR MSISDN.
       
       FIND FIRST bfpcMSISDN EXCLUSIVE-LOCK
            WHERE bfpcMSISDN.brand      EQ Syst.Var:gcBrand 
              AND bfpcMSISDN.ValidTo    GE {&nowts}
              AND bfpcMSISDN.cli        EQ THIS-OBJECT:lcCLI
              AND bfpcMSISDN.statuscode EQ 1 
           NO-ERROR.
       
          
       mMakeMSISDNHistoryTimeStamp(RECID(bfpcMSISDN) , Func.Common:mMakeTS()).
       
       ASSIGN bfpcMSISDN.statuscode     = 2
              bfpcMSISDN.orderid        = THIS-OBJECT:OrderId.
       
   END METHOD.
   
   METHOD PUBLIC VOID mMakeMSISDNHistoryTimeStamp(INPUT irREcID      AS RECID ,
                                                  INPUT idTimeStamp  AS DECIMAL) :
       
       DEFINE VARIABLE ldNewFrom  AS DECIMAL  NO-UNDO.
       DEFINE VARIABLE ldtNewDate AS DATE     NO-UNDO.
       DEFINE VARIABLE liNewTime  AS INTEGER  NO-UNDO.
       
       DEF BUFFER HistMSISDN FOR MSISDN.
   
       FIND FIRST HistMSISDN WHERE 
           RECID(HistMSISDN) = irREcID EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
           
       IF NOT AVAILABLE HistMSISDN THEN RETURN.
          
       IF idTimeStamp = 0 OR idTimeStamp = ? THEN idTimeStamp = Func.Common:mMakeTS().
   
       ASSIGN HistMSISDN.ValidTo = idTimeStamp.

       ldNewFrom = HistMSISDN.ValidTo.
   
       /* make sure that there is atleast 1 second gap between rows */
       REPEAT:
           /* do this first so that ldtnewdate is available for ActionDate */
           Func.Common:mSplitTS(ldNewFrom,
                                OUTPUT ldtNewDate,
                                OUTPUT liNewTime).

           IF NOT CAN-FIND(FIRST MSISDN WHERE
                                 MSISDN.Brand     = HistMSISDN.Brand AND
                                 MSISDN.CLI       = HistMSISDN.CLI   AND
                                 MSISDN.ValidFrom = ldNewFrom)
           THEN LEAVE.

           IF liNewTime >= 86400 THEN 
           ASSIGN  ldtNewDate = ldtNewDate + 1
                   liNewTime  = 1.
           ELSE liNewTime = liNewTime + 1.
      
           ldNewFrom = Func.Common:mMake2DT(ldtNewDate,liNewTime).
       END.
   
       CREATE MSISDN.
       BUFFER-COPY HistMSISDN EXCEPT ValidFrom validTo TO MSISDN.
       ASSIGN
           MSISDN.ValidFrom  = ldNewFrom
           MSISDN.ValidTo    = 99999999.99999
           MSISDN.ActionDate = ldtNewDate.
                                                      
                                                      
   END METHOD.        
   
   METHOD PUBLIC VOID mCompanyCustomerStatus() :
       
       DEFINE BUFFER bfpcCustomer2 FOR Customer.
       DEFINE BUFFER bfpcMobSub2   FOR MobSub.
       
       IF THIS-OBJECT:aoCustomer:isNewCustomer
       THEN ASSIGN THIS-OBJECT:StatusCode   = {&ORDER_STATUS_COMPANY_NEW}.
       ELSE DO:
           
           FIND FIRST bfpcCustomer2 WHERE
                      bfpcCustomer2.Brand      = Syst.Var:gcBrand                  AND
                      bfpcCustomer2.OrgId      = THIS-OBJECT:aoCustomer:CustId     AND
                      bfpcCustomer2.CustIdType = THIS-OBJECT:aoCustomer:CustIdType AND
                      bfpcCustomer2.Roles      NE "inactive" NO-LOCK NO-ERROR. 
           IF AVAIL bfpcCustomer2 THEN 
           DO:
               FIND FIRST bfpcMobSub2 WHERE
                          bfpcMobSub2.Brand   = Syst.Var:gcBrand AND
                          bfpcMobSub2.AgrCust = bfpcCustomer2.CustNum
               NO-LOCK NO-ERROR.
               
               IF NOT AVAIL bfpcMobSub2 THEN THIS-OBJECT:StatusCode = {&ORDER_STATUS_COMPANY_NEW}.
               ELSE THIS-OBJECT:StatusCode = {&ORDER_STATUS_COMPANY_MNP}.
           END. /* IF AVAIL Customer THEN DO: */
           
       END.
       
   END METHOD.    
   
   METHOD PUBLIC VOID mCreateChangeICCRequest() :
       
       DEFINE VARIABLE liRequest AS INTEGER   NO-UNDO.
       DEFINE VARIABLE lcError   AS CHARACTER NO-UNDO.
       
       IF THIS-OBJECT:OrderChannel  BEGINS "Renewal_POS" AND 
          THIS-OBJECT:lcICC         GT      ""           AND 
          THIS-OBJECT:ROIClose      EQ      FALSE 
       THEN DO:
           
           liRequest  =  DYNAMIC-FUNCTION ("fSubscriptionRequest" IN lhOrderValidations ,
                                           INPUT THIS-OBJECT:FinalMsSeq ,
                                           INPUT THIS-OBJECT:lcCLI ,
                                           INPUT THIS-OBJECT:aoCustomer:CustNum,
                                           INPUT 1,
                                           INPUT "",
                                           INPUT Func.Common:mMakeTS(),
                                           INPUT  "CHANGEICC",
                                           INPUT  THIS-OBJECT:lcICC,
                                           INPUT "",
                                           INPUT "",
                                           INPUT "",
                                           INPUT FALSE,
                                           INPUT 0.0,
                                           INPUT {&REQUEST_SOURCE_ICC_CHANGE_AUTO},
                                           OUTPUT lcError).
                                           
                                                    
           IF liRequest = 0
           THEN Func.Common:mWriteMemoWithType("Order",
                                               STRING(THIS-OBJECT:OrderId),
                                               0,
                                               "ICC change request creation failed",
                                               SUBST("Orderid: &1", Order.orderid),
                                               "",
                                               "BSSAPI").
           ELSE DO:
               
               mUpdateSimStatus(THIS-OBJECT:lcICC).
               
               Func.Common:mWriteMemo("MsRequest",
                                      STRING(liRequest),
                                      THIS-OBJECT:aoCustomer:CustNum,
                                      "ICC TYPE CHANGE AUTO",
                                     THIS-OBJECT:OrderChannel).
           END.                                    
       END.
       
    END METHOD.
   
    METHOD PUBLIC VOID mUpdateSimStatus(INPUT lcICC AS CHARACTER ):
        
        DEFINE BUFFER bfSIM FOR SIM.
        
        FIND FIRST bfSIM WHERE bfSIM.Brand   =  Syst.Var:gcBrand
                           AND bfSIM.ICC     =  THIS-OBJECT:lcICC 
                           AND bfSIM.SimStat =  1
            EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
                           
        IF AVAILABLE bfSIM 
        THEN DO:
            
            IF THIS-OBJECT:MobileNumberType = "RENEWAL"
            THEN ASSIGN bfSIM.SimStat   = 13.
            ELSE ASSIGN bfSIM.SimStat   =  4.
            
        END.
        ELSE UNDO, THROW NEW ParamError("Unable to update the SIM Status.").
        
    END METHOD.
                                             
    
END CLASS.
