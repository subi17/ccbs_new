 
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

CLASS om.Order IMPLEMENTS bss.cls.IObjectStorage:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    DEFINE PUBLIC PROPERTY objCustomer AS CLASS cm.Customer NO-UNDO
        GET.
        SET.
        
    DEFINE PUBLIC PROPERTY ObjCustomerAcct AS CLASS cm.CustomerAccount NO-UNDO 
        GET.
        SET.
       
    DEFINE PUBLIC PROPERTY objOrderInspection AS CLASS om.OrderInspection NO-UNDO 
        GET.
        SET.
      
    DEFINE PUBLIC PROPERTY objOrderPayment AS CLASS om.OrderPayment NO-UNDO
        GET.
        SET.
        
    DEFINE PUBLIC PROPERTY objOrderProductFactory AS CLASS pm.OrderProductFactory NO-UNDO
        GET.
        SET.  
        
    DEFINE PUBLIC PROPERTY ObjPaymentStruct AS CLASS JsonObject NO-UNDO
        GET.
        SET.    
    
    DEFINE PUBLIC PROPERTY ObjInspectionStruct AS CLASS JsonObject NO-UNDO
        GET.
        SET.
        
    DEFINE PUBLIC PROPERTY ObjCustomerStruct AS CLASS JsonObject NO-UNDO
        GET.
        SET.
       
    DEFINE PUBLIC PROPERTY orderId AS INTEGER NO-UNDO
        GET.
        SET.
        
    DEFINE PUBLIC PROPERTY custAcctNum AS INTEGER NO-UNDO
        GET.
        SET.
        
    DEFINE PUBLIC PROPERTY orderType AS INTEGER NO-UNDO
        GET.
        SET.
        
    DEFINE PUBLIC PROPERTY mobSubCustNum AS INTEGER NO-UNDO
        GET.
        SET.
        
    DEFINE PUBLIC PROPERTY finalMsSeq AS INTEGER NO-UNDO
        GET.
        SET.
        
    DEFINE PUBLIC PROPERTY crStamp AS DATETIME-TZ NO-UNDO
        GET.
        SET.        
        
    DEFINE PUBLIC PROPERTY createdStamp AS DECIMAL NO-UNDO
        GET.
        SET.        

    DEFINE PUBLIC PROPERTY contractId AS CHARACTER NO-UNDO
        GET.
        SET.

    DEFINE PUBLIC PROPERTY salesMan AS CHARACTER NO-UNDO
        GET.
        SET.

    DEFINE PUBLIC PROPERTY orderChannel AS CHARACTER NO-UNDO
        GET.
        SET.
   
    DEFINE PUBLIC PROPERTY CLI AS CHARACTER NO-UNDO
        GET.
        SET.
      
    DEFINE PUBLIC PROPERTY lcCLI AS CHARACTER NO-UNDO
        GET.
        SET.
       
    DEFINE PUBLIC PROPERTY statusCode AS CHARACTER NO-UNDO
        GET.
        SET.
        
    DEFINE PUBLIC PROPERTY mobileNumberType AS CHARACTER NO-UNDO
        GET.
        SET.
        
    DEFINE PUBLIC PROPERTY roiOrderStatus AS CHARACTER NO-UNDO
        GET.
        SET.
        
    DEFINE PUBLIC PROPERTY cliType AS CHARACTER NO-UNDO
        GET.
        SET.
       
    DEFINE PUBLIC PROPERTY lcICC AS CHARACTER NO-UNDO
        GET.
        SET.
      
    DEFINE PUBLIC PROPERTY roiClose AS LOGICAL NO-UNDO
        GET.
        SET.
      
    DEFINE PUBLIC PROPERTY isACCOrder AS LOGICAL NO-UNDO
        GET.
        SET.   
        
    DEFINE PUBLIC PROPERTY isPrePaidOrder AS LOGICAL NO-UNDO
        GET.
        SET.
        
    DEFINE PUBLIC PROPERTY isFixedOnly AS LOGICAL NO-UNDO
        GET.
        SET.
        
    DEFINE PUBLIC PROPERTY isRenewalSTCOrder AS LOGICAL NO-UNDO
        GET.
        SET.
        
    DEFINE PUBLIC PROPERTY custAcctid AS INTEGER NO-UNDO
       GET.
       SET.
       
    DEFINE PUBLIC PROPERTY objBillAcctArray AS CLASS JsonArray  NO-UNDO
       GET.
       SET.
       
    DEFINE PUBLIC PROPERTY payType AS LOGICAL NO-UNDO
       GET.
       SET.
       
    /* A value for this property is populated in OrderProduct class */
    DEFINE PUBLIC PROPERTY msSeq AS INTEGER NO-UNDO
        GET.
        SET(iiMSSeq AS INTEGER):
            DEFINE BUFFER MobSub FOR MobSub.
            FIND MobSub NO-LOCK WHERE MobSub.MSSeq = iiMSSeq NO-ERROR.
            IF NOT AVAILABLE MobSub THEN
            DO:
                IF CAN-FIND(TermMobSub NO-LOCK WHERE TermMobSub.MSSeq = iiMSSeq) THEN 
                    UNDO, THROW NEW ParamError(SUBSTITUTE("subscription_not_active|&1", iiMSSeq)).

                UNDO, THROW NEW ParamError(SUBSTITUTE("subscription_not_found|&1", iiMSSeq)).
            END.
            ASSIGN
                THIS-OBJECT:msSeq         = iiMSSeq
                THIS-OBJECT:CLI           = MobSub.CLI
                THIS-OBJECT:mobSubCustNum = MobSub.CustNum.
        END SET.
       
    DEFINE PUBLIC PROPERTY lhOrderLibrary AS HANDLE NO-UNDO 
        GET.
        SET.
       
	CONSTRUCTOR PUBLIC Order():
	    
	END CONSTRUCTOR.

	METHOD PUBLIC VOID mCreateData(ioJsonObject AS CLASS JsonObject):

        DEFINE VARIABLE loAgrCust      AS CLASS JsonObject NO-UNDO.
        DEFINE VARIABLE ldaCreateDate  AS DATETIME-TZ      NO-UNDO.
        DEFINE VARIABLE liCount        AS INTEGER          NO-UNDO.
        DEFINE VARIABLE lcMigrationErr AS CHARACTER        NO-UNDO.        
              
        ASSIGN
            loAgrCust         = ioJsonObject:GetJsonObject("agreement-customer") WHEN ioJsonObject:Has("agreement-customer")
            ObjCustomerStruct = ioJsonObject:GetJsonObject("customer")           WHEN ioJsonObject:Has("customer")
            objCustomer       = NEW cm.Customer()
            ObjCustomerAcct   = NEW cm.CustomerAccount(objCustomer).

        ASSIGN
            THIS-OBJECT:crStamp       = ioJsonObject:GetDatetimeTZ("created_at")
            THIS-OBJECT:ContractId    = ioJsonObject:GetCharacter("contractid") 
            THIS-OBJECT:salesMan      = ioJsonObject:GetCharacter("salesman")
            THIS-OBJECT:orderChannel  = ioJsonObject:GetCharacter("channel")
            objCustomer:isProCustomer = ( INDEX(THIS-OBJECT:orderChannel,"PRO") > 0 )     
            objCustomer:orderChannel  = THIS-OBJECT:orderChannel                   
            .

        IF VALID-OBJECT(loAgrCust) THEN 
        DO:          
            IF loAgrCust:Has("custnum") THEN 
                objCustomer:mFindCustomer(loAgrCust:GetInteger("custnum")).
            ELSE 
                objCustomer:mCreateData(loAgrCust).
          
            ASSIGN 
                THIS-OBJECT:isACCOrder = TRUE.
        END. 
        ELSE IF VALID-OBJECT(ObjCustomerStruct) THEN 
        DO:
            ASSIGN 
                ObjPaymentStruct    = ioJsonObject:GetJsonObject("upfront_payment_method") WHEN ioJsonObject:Has("upfront_payment_method")
                ObjInspectionStruct = ioJsonObject:GetJsonObject("inspection")             WHEN ioJsonObject:Has("inspection").
      
            IF ObjCustomerStruct:Has("account_id") THEN 
            DO:              
                ASSIGN 
                    THIS-OBJECT:custAcctNum = ObjCustomerStruct:GetInteger("account_id").
                    
                objCustomerAcct:mFindCustomerAcct(THIS-OBJECT:custAcctNum).
                
                ASSIGN 
                    THIS-OBJECT:custAcctid = objCustomerAcct:mGetCustomerAcctId().
                     
                objCustomer:mFindCustomer(objCustomerAcct:mGetCustNum()).     
            END.    
            ELSE objCustomer:mCreateData(INPUT ObjCustomerStruct).
      
            IF ObjCustomerStruct:Has("billing_accounts") THEN
            DO:             
                ASSIGN 
                    objBillAcctArray = ObjCustomerStruct:GetJsonArray("billing_accounts"). 
                objCustomerAcct:mCreateBillingAcct(objBillAcctArray). 
                objCustomer:mAssignBillAcctIdData(ObjCustomerAcct:mGetBillAcctIDData()).     
            END.          
        END.            
  
        RUN Mc/libfunc.p PERSISTENT SET lhOrderLibrary.  /* This is for the complicated business logic */
      
        IF THIS-OBJECT:orderChannel BEGINS "migration"
            AND THIS-OBJECT:isACCOrder  = FALSE THEN 
        DO:
            ASSIGN 
                lcMigrationErr = DYNAMIC-FUNCTION("fMigrationCheckCustomer" IN THIS-OBJECT:lhOrderLibrary , INPUT Syst.Var:gcBrand , INPUT objCustomer:CustId).
            IF lcMigrationErr > "" THEN
                UNDO , THROW NEW ParamError("Migration data validation error:" + lcMigrationErr).
        END.
      
        IF VALID-OBJECT(ObjInspectionStruct) THEN
        DO:
            ASSIGN 
                objOrderInspection = NEW om.OrderInspection(THIS-OBJECT).
            objOrderInspection:mCreateData(ObjInspectionStruct).
        END.
        
        IF VALID-OBJECT(ObjPaymentStruct) THEN
        DO:
            ASSIGN 
                objOrderPayment = NEW om.OrderPayment(THIS-OBJECT).
            objOrderPayment:mCreateData(ObjPaymentStruct).
        END.      
      
        IF ioJsonObject:Has("items") THEN
        DO:
            objOrderProductFactory = NEW pm.OrderProductFactory().
            objOrderProductFactory:mCreateData(ioJsonObject:GetJsonArray("items"), THIS-OBJECT).
        END.
        ELSE UNDO, THROW NEW ParamError("order_items_are_mandatory").
    
	END METHOD.

    METHOD PUBLIC VOID mStoreData():
        
        IF objCustomer:isNewCustomer OR objCustomer:isCustomerUpdate THEN
            objCustomer:mStoreData().
        
        IF THIS-OBJECT:lcICC > "" THEN 
            mUpdateSimStatus(THIS-OBJECT:lcICC).
            
        CREATE Order.
        ASSIGN
            Order.Brand              = Syst.Var:gcBrand
            Order.OrderId            = NEXT-VALUE(OrderId)
            Order.Source             = "bssapi"
            Order.CustNum            = objCustomer:CustNum
            Order.MsSeq              = (IF (THIS-OBJECT:isACCOrder OR THIS-OBJECT:isRenewalSTCOrder) THEN THIS-OBJECT:msSeq ELSE NEXT-VALUE(MobSub))
            THIS-OBJECT:finalMsSeq   = Order.MsSeq
            Order.CLI                = (IF THIS-OBJECT:isACCOrder THEN THIS-OBJECT:CLI   ELSE THIS-OBJECT:lcCLI)
            Order.contractId         = THIS-OBJECT:contractId
            Order.SalesMan           = THIS-OBJECT:salesMan
            Order.OrderChannel       = THIS-OBJECT:orderChannel
            Order.CrStamp            = Func.Common:mDateTimeTZ2TS(THIS-OBJECT:crStamp)
            THIS-OBJECT:createdStamp = Order.CrStamp
            THIS-OBJECT:orderId      = Order.OrderId
            Order.MNPStatus          = (INTEGER(THIS-OBJECT:mobileNumberType = {&TYPE_MNP})).
            
        CASE THIS-OBJECT:mobileNumberType:
            
            WHEN {&TYPE_NEW}       THEN Order.OrderType = {&ORDER_TYPE_NEW}.
            WHEN {&TYPE_MNP}       THEN Order.OrderType = {&ORDER_TYPE_MNP}.
            WHEN {&TYPE_RENEWAL}   THEN Order.OrderType = {&ORDER_TYPE_RENEWAL}.
            WHEN {&TYPE_STC}       THEN Order.OrderType = {&ORDER_TYPE_STC}.
            WHEN {&TYPE_MIGRATION} THEN Order.OrderType = {&ORDER_TYPE_MNP}.
            
        END.
            
        IF VALID-OBJECT(ObjInspectionStruct) THEN
            objOrderInspection:mStoreData().
            
        IF VALID-OBJECT(ObjPaymentStruct) THEN
            objOrderPayment:mStoreData().
        
        IF THIS-OBJECT:isACCOrder = FALSE THEN
        DO:
            IF THIS-OBJECT:lcCLI > "" THEN
            DO:
                mCloseOfferedOrderWithSameNumber().
                mCheckDuplicateOrders().
            END. 
            
            IF THIS-OBJECT:mobileNumberType = {&TYPE_NEW} AND THIS-OBJECT:roiClose = FALSE THEN
                mMakeMSISDNHistory().
            
            IF THIS-OBJECT:roiClose = FALSE THEN 
            DO:
           
                IF THIS-OBJECT:statusCode    <> {&ORDER_STATUS_IN_CONTROL} AND 
                   THIS-OBJECT:orderChannel  BEGINS "RETENTION" OR 
                   (Func.ValidateOrder:mIsConvergentTariff(THIS-OBJECT:cliType) AND THIS-OBJECT:mobileNumberType = "STC") AND 
                   Mnp.MNPOutGoing:mIsMNPOutOngoing(INPUT THIS-OBJECT:lcCLI) THEN 
                DO:
                     ASSIGN THIS-OBJECT:statusCode  =  {&ORDER_STATUS_MNP_RETENTION}.
                     
                     IF THIS-OBJECT:orderChannel = "retention_stc" THEN 
                     DO:
                         FIND FIRST MsRequest WHERE
                                    MsRequest.MsSeq = Order.MsSeq AND
                                    MsRequest.ReqType = 0         AND
                             LOOKUP(STRING(MsRequest.ReqStatus),"2,4,9") = 0
                             EXCLUSIVE-LOCK NO-ERROR.
                         IF AVAIL MsRequest THEN
                             MsRequest.ReqIParam2 = THIS-OBJECT:orderId.
                         ELSE DO:
                             THIS-OBJECT:statusCode = {&ORDER_STATUS_IN_CONTROL}.
                             Func.Common:mWriteMemoWithType("Order",
                                                            STRING(THIS-OBJECT:orderId),
                                                            0,
                                                            "STC request was not found",
                                                            "",
                                                            "",
                                                            "BSSAPI").
                         END. /* ELSE DO: */
                     END. /* IF Order.orderChannel = "retention_stc" THEN DO: */
                END. /* IF THIS-OBJECT:statusCode    <> {&ORDER_STATUS_IN_CONTROL} AND  */
                ELSE IF THIS-OBJECT:statusCode <> {&ORDER_STATUS_IN_CONTROL} THEN
                DO:                    
                    /*TO-DO PC: PLEASE IMPLEMENT BELOW LOGIC FROM WHOLE ORDER RPC*/
                    
                    /*IF pcNumberType EQ "renewal" THEN  ==> NOT IMPLEMENTED THIS LOGIC*/   
                    
                    IF THIS-OBJECT:objCustomer:CustIdType = "CIF" THEN 
                        mCompanyCustomerStatus().
                    ELSE IF THIS-OBJECT:orderChannel BEGINS "fusion" THEN
                        ASSIGN THIS-OBJECT:statusCode = {&ORDER_STATUS_PENDING_FIXED_LINE}.
                    ELSE DO:
                                                
                        IF THIS-OBJECT:mobileNumberType     = {&TYPE_MNP} OR THIS-OBJECT:mobileNumberType     = {&TYPE_MIGRATION} THEN
                            ASSIGN THIS-OBJECT:statusCode  = {&ORDER_STATUS_MNP}.
                        ELSE 
                            ASSIGN THIS-OBJECT:statusCode  = {&ORDER_STATUS_NEW}.
                        
                        /*TO-DO PC : STATUS Change based on the multi sim type and multi sim id*/
                        /*TO-DO PC : STATUS Change based on pending main line and porting date*/
                        
                    END. /* ELSE DO: */                    
                END. /* ELSE IF THIS-OBJECT:statusCode <> {&ORDER_STATUS_IN_CONTROL} */                     
            END. /* IF THIS-OBJECT:roiClose    = FALSE  */
            
            IF THIS-OBJECT:roiOrderStatus > "" THEN
                ASSIGN THIS-OBJECT:statusCode = THIS-OBJECT:roiOrderStatus.
                
            IF THIS-OBJECT:lcICC  GT "" THEN  
                mCreateChangeICCRequest().
                
        END. /*IF THIS-OBJECT:isACCOrder = FALSE */

        ASSIGN 
            Order.StatusCode    = THIS-OBJECT:statusCode
            Order.Paytype       = (IF Func.ValidateOrder:mGetPayType(INPUT THIS-OBJECT:cliType) = 2 THEN YES ELSE NO) 
            Order.AccountId     = objCustomer:custAcctID
            Order.CLIType       = THIS-OBJECT:cliType
            THIS-OBJECT:payType = Order.Paytype.
            
        IF VALID-OBJECT(objBillAcctArray) THEN 
            ObjCustomerAcct:mStoreBillingAcct().
        
        objOrderProductFactory:mStoreData().    
        
    END METHOD.
    
    METHOD PUBLIC VOID mCloseOfferedOrderWithSameNumber():
        
        DEFINE BUFFER bfpcOrder FOR Order.
        
        IF THIS-OBJECT:orderChannel BEGINS "RETENTION" THEN
        DO:
            
            FOR EACH bfpcOrder NO-LOCK WHERE
                     bfpcOrder.Brand      =     Syst.Var:gcBrand           AND
                     bfpcOrder.CLI        =     THIS-OBJECT:lcCLI          AND
                     bfpcOrder.StatusCode =     {&ORDER_STATUS_OFFER_SENT} AND  /* shouldn't never get this value because of YDR-2575 */
                     ROWID(bfpcOrder)     <>    ROWID(Order):

                RUN Mc/closeorder.p(bfpcOrder.OrderId, TRUE) NO-ERROR.

                IF RETURN-VALUE > "" THEN
                    Func.Common:mWriteMemoWithType("Order",
                                                    STRING(THIS-OBJECT:orderId),
                                                    0,
                                                    "Automatic order closing failed",
                                                    SUBST("Failed to close pending order. " + 
                                                    "Order ID: &1, Error: &2", 
                                                    bfpcOrder.orderid, RETURN-VALUE),
                                                    "",
                                                    "Newton RPC").
                                                        
            END. /* FOR EACH bfpcOrder */
        END. /* IF THIS-OBJECT:orderChannel */
        
    END METHOD.

   METHOD PUBLIC JsonObject mResult():

       DEFINE VARIABLE loJsonObject  AS CLASS JsonObject NO-UNDO.
       DEFINE VARIABLE loCustObject  AS CLASS JsonObject NO-UNDO.
       DEFINE VARIABLE loCustAcctobj AS CLASS JsonObject NO-UNDO.
      
       loJsonObject = NEW JsonObject().
       loJsonObject:Add("order-id", THIS-OBJECT:orderId).
      
       IF objCustomer:isNewCustomer THEN
       DO:
           ASSIGN 
               loCustObject = NEW JsonObject()
               loCustObject = objCustomer:mResult().
           loJsonObject:Add("customer" , loCustObject). 
       END.
      
       IF VALID-OBJECT(objBillAcctArray) THEN 
       DO:
           ASSIGN 
               loCustObject  = NEW JsonObject()
               loCustAcctobj = NEW JsonObject()
               loCustAcctobj = ObjCustomerAcct:mResult().
              
           loCustObject:Add("cust-num",objCustomer:custNum).     
           loCustObject:ADD("customer-accounts",loCustAcctobj).
           loJsonObject:Add("customer",loCustObject).
       END.
       
       objOrderProductFactory:mResult(loJsonObject).

       RETURN loJsonObject.

   END METHOD.
   
   METHOD PUBLIC VOID mCheckDuplicateOrders():
       
       IF Func.ValidateOrder:mCheckOtherOngoingOrders(INPUT THIS-OBJECT:lcCLI , INPUT THIS-OBJECT:mobileNumberType) THEN
       DO:
           ASSIGN THIS-OBJECT:statusCode  =  {&ORDER_STATUS_IN_CONTROL}.
           
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
              bfpcMSISDN.orderid        = THIS-OBJECT:orderId.
       
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
          
       IF idTimeStamp = 0 OR idTimeStamp = ? THEN 
           idTimeStamp = Func.Common:mMakeTS().
   
       ASSIGN HistMSISDN.ValidTo = idTimeStamp
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
                                 MSISDN.ValidFrom = ldNewFrom) THEN
               LEAVE.

           IF liNewTime >= 86400 THEN 
           ASSIGN  ldtNewDate = ldtNewDate + 1
                   liNewTime  = 1.
           ELSE liNewTime = liNewTime + 1.
      
           ASSIGN 
               ldNewFrom = Func.Common:mMake2DT(ldtNewDate,liNewTime).
       END.
   
       CREATE MSISDN.
       BUFFER-COPY HistMSISDN EXCEPT ValidFrom validTo TO MSISDN.
       ASSIGN
           MSISDN.ValidFrom  = ldNewFrom
           MSISDN.ValidTo    = 99999999.99999
           MSISDN.ActionDate = ldtNewDate
           MSISDN.LockedTo   = Func.Common:mSecOffSet(Func.Common:mMakeTS(),60).
                                                      
                                                      
   END METHOD.        
   
   METHOD PUBLIC VOID mCompanyCustomerStatus() :
       
       DEFINE BUFFER bfpcCustomer2 FOR Customer.
       DEFINE BUFFER bfpcMobSub2   FOR MobSub.
       
       IF THIS-OBJECT:objCustomer:isNewCustomer THEN
           ASSIGN THIS-OBJECT:statusCode   = {&ORDER_STATUS_COMPANY_NEW}.
       ELSE DO:
           
           FIND FIRST bfpcCustomer2 WHERE
                      bfpcCustomer2.Brand      = Syst.Var:gcBrand                  AND
                      bfpcCustomer2.OrgId      = THIS-OBJECT:objCustomer:CustId     AND
                      bfpcCustomer2.CustIdType = THIS-OBJECT:objCustomer:CustIdType AND
                      bfpcCustomer2.Roles      NE "inactive" NO-LOCK NO-ERROR. 
           IF AVAIL bfpcCustomer2 THEN 
           DO:
               FIND FIRST bfpcMobSub2 WHERE
                          bfpcMobSub2.Brand   = Syst.Var:gcBrand AND
                          bfpcMobSub2.AgrCust = bfpcCustomer2.CustNum
               NO-LOCK NO-ERROR.
               
               IF NOT AVAIL bfpcMobSub2 THEN THIS-OBJECT:statusCode = {&ORDER_STATUS_COMPANY_NEW}.
               ELSE THIS-OBJECT:statusCode = {&ORDER_STATUS_COMPANY_MNP}.
           END. /* IF AVAIL Customer THEN DO: */
           
       END.
       
   END METHOD.    
   
   METHOD PUBLIC VOID mCreateChangeICCRequest() :
       
       DEFINE VARIABLE liRequest AS INTEGER   NO-UNDO.
       DEFINE VARIABLE lcError   AS CHARACTER NO-UNDO.
       
       IF THIS-OBJECT:orderChannel  BEGINS "Renewal_POS" AND 
          THIS-OBJECT:lcICC         GT      ""           AND 
          THIS-OBJECT:roiClose      EQ      FALSE THEN
       DO:
           
           liRequest  =  DYNAMIC-FUNCTION ("fSubscriptionRequest" IN lhOrderLibrary ,
                                           INPUT THIS-OBJECT:finalMsSeq ,
                                           INPUT THIS-OBJECT:lcCLI ,
                                           INPUT THIS-OBJECT:objCustomer:CustNum,
                                           INPUT 1,
                                           INPUT "",
                                           INPUT Func.Common:mMakeTS(),
                                           INPUT  "CHANGEICC",
                                           INPUT  THIS-OBJECT:lcICC,
                                           INPUT "",
                                           INPUT "",
                                           INPUT "",
                                           INPUT "",
                                           INPUT FALSE,
                                           INPUT 0.0,
                                           INPUT {&REQUEST_SOURCE_ICC_CHANGE_AUTO},
                                           OUTPUT lcError).
                                           
                                                    
           IF liRequest = 0 THEN
               Func.Common:mWriteMemoWithType("Order",
                                               STRING(THIS-OBJECT:orderId),
                                               0,
                                               "ICC change request creation failed",
                                               SUBST("Orderid: &1", Order.orderid),
                                               "",
                                               "BSSAPI").
           ELSE 
           DO:               
               mUpdateSimStatus(THIS-OBJECT:lcICC).
               
               Func.Common:mWriteMemo("MsRequest",
                                       STRING(liRequest),
                                       THIS-OBJECT:objCustomer:CustNum,
                                       "ICC TYPE CHANGE AUTO",
                                       THIS-OBJECT:orderChannel).
           END.                                    
       END.
       
    END METHOD.
   
    METHOD PUBLIC VOID mUpdateSimStatus(INPUT lcICC AS CHARACTER ):
        
        DEFINE BUFFER bfSIM FOR SIM.
        
        FIND FIRST bfSIM WHERE bfSIM.Brand   =  Syst.Var:gcBrand
                           AND bfSIM.ICC     =  THIS-OBJECT:lcICC 
                           AND bfSIM.SimStat =  1
            EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
                           
        IF AVAILABLE bfSIM THEN
        DO:
            
            IF THIS-OBJECT:mobileNumberType = "RENEWAL" THEN 
                ASSIGN 
                    bfSIM.SimStat   = 13.
            ELSE 
                ASSIGN bfSIM.SimStat   =  4.
            
        END.
        ELSE 
            UNDO, THROW NEW ParamError("Unable to update the SIM Status.").
        
    END METHOD.
    
    DESTRUCTOR PUBLIC Order():

        IF VALID-OBJECT(objCustomer) THEN
            DELETE OBJECT objCustomer.

        IF VALID-OBJECT(objOrderProductFactory) THEN 
            DELETE OBJECT objOrderProductFactory.
            
        IF VALID-OBJECT(ObjPaymentStruct) THEN 
            DELETE OBJECT ObjPaymentStruct.
        
        IF VALID-OBJECT(ObjInspectionStruct) THEN 
            DELETE OBJECT ObjInspectionStruct.
            
        IF VALID-OBJECT(ObjCustomerStruct) THEN 
            DELETE OBJECT ObjCustomerStruct.
        
        IF VALID-OBJECT(objOrderInspection) THEN 
            DELETE OBJECT objOrderInspection.
            
        IF VALID-OBJECT(objOrderPayment) THEN 
            DELETE OBJECT objOrderPayment.
            
    END DESTRUCTOR.
    
END CLASS.
