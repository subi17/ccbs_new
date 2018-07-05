USING Progress.Json.ObjectModel.*.

{Syst/tmsconst.i}
{Func/log.i}
{Func/masmovileif.i}
{Mc/orderfusion.i}
{Syst/eventval.i}
{Func/msreqfunc.i}
{Func/cparam2.i}

&GLOBAL-DEFINE WAIT_CONFIG_LOCK 20

DEF INPUT PARAM piMessageSeq     AS INT        NO-UNDO.

/* Parsing address data to JSON */
DEF VAR lobjOuterObject          AS JsonObject NO-UNDO.
DEF VAR lcServicesArray          AS JsonArray  NO-UNDO.
DEF VAR lcServicesObject         AS JsonObject NO-UNDO.
DEF VAR lcCharacteristicsArray   AS JsonArray  NO-UNDO.
DEF VAR lcCharacteristicsObject1 AS JsonObject NO-UNDO.
DEF VAR lcCharacteristicsObject2 AS JsonObject NO-UNDO.
DEF VAR lcCharacteristicsObject3 AS JsonObject NO-UNDO.
DEF VAR lcInstallationObject     AS JsonObject NO-UNDO.
DEF VAR lcAddressObject          AS JsonObject NO-UNDO.
DEF VAR loJson                   AS JsonObject NO-UNDO.
   
DEF VAR lcHost                   AS CHAR       NO-UNDO.
DEF VAR liPort                   AS INT        NO-UNDO.
DEF VAR lcUserId                 AS CHAR       NO-UNDO.
DEF VAR lcpassword               AS CHAR       NO-UNDO.
DEF VAR lcUriPath                AS CHAR       NO-UNDO.
DEF VAR lcUriQuery               AS CHAR       NO-UNDO.
DEF VAR lcUriQueryVal            AS CHAR       NO-UNDO.
DEF VAR liLogRequest             AS INT        NO-UNDO.
DEF VAR llLogRequest             AS LOGICAL    NO-UNDO INIT TRUE.

DEF VAR lcSalesManId             AS CHAR       NO-UNDO.
DEF VAR liOrderId                AS INT        NO-UNDO.
DEF VAR lcAmendamentType         AS CHAR       NO-UNDO.
DEF VAR lcAmendamentValue        AS CHAR       NO-UNDO.
DEF VAR lcContractId             AS CHAR       NO-UNDO.
DEF VAR lcReason                 AS CHAR       NO-UNDO.
DEF VAR lcCurrentDetails         AS CHAR       NO-UNDO.

DEF VAR lcResultCode             AS CHAR       NO-UNDO.
DEF VAR lcResultDesc             AS CHAR       NO-UNDO. 
DEF VAR lcJsonResult             AS CHAR       NO-UNDO.

/* Update Installation Address */
DEF VAR llUpdateAL               AS LOGICAL    NO-UNDO.
DEF VAR lcTableName              AS CHAR       NO-UNDO.
DEF VAR lcActionID               AS CHAR       NO-UNDO.
DEF VAR ldCurrentTimeTS          AS DEC        NO-UNDO.
DEF VAR lcMemo                   AS CHAR       NO-UNDO.

DEF VAR liWait                   AS INT        NO-UNDO.
DEF VAR lhOrdCustomer            AS HANDLE     NO-UNDO.
DEF VAR lcError                  AS CHAR       NO-UNDO. 
DEF VAR ldaOrigCancelDate        AS DATE       NO-UNDO.
      
FIND FusionMessage EXCLUSIVE-LOCK WHERE
     FusionMessage.MessageSeq = piMessageSeq NO-WAIT NO-ERROR.
     
IF FusionMessage.MessageType NE {&FUSIONMESSAGE_TYPE_ADDRESS_CHANGE} THEN
   RETURN SUBST("Incorrect message type: &1", FusionMessage.MessageType).
   
FIND Order EXCLUSIVE-LOCK WHERE
     Order.Brand = Syst.Var:gcBrand AND
     Order.OrderId = FusionMessage.OrderID 
     NO-WAIT NO-ERROR.

FIND OrderFusion EXCLUSIVE-LOCK WHERE
     OrderFusion.Brand = Syst.Var:gcBrand AND
     OrderFusion.OrderID = FusionMessage.OrderID
     NO-WAIT NO-ERROR.

IF NOT OrderFusion.FixedNumber > "" THEN
   RETURN fFusionMessageError(BUFFER FusionMessage,
                             "FixedNumber not assigned").

FIND FIRST MSRequest EXCLUSIVE-LOCK WHERE 
           MSRequest.MsSeq = FusionMessage.Msseq AND
           MSRequest.ReqType = {&REQTYPE_INSTALL_ADDRESS_UPDATE} 
           NO-WAIT NO-ERROR.
IF AVAIL MSRequest THEN DO: 
   ASSIGN 
      lcSalesManId = MSRequest.ReqCParam1
      liOrderId = MSRequest.ReqIParam1 
      lcAmendamentType = MSRequest.ReqCParam2 
      lcAmendamentValue = MSRequest.ReqCParam3 
      lcCurrentDetails = MSRequest.ReqCParam4 
      lcContractId = MSRequest.ReqCParam5 
      lcReason = MSRequest.ReqCParam6.
END.  

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER Syst.Var:katun
   {Func/lib/eventlog.i}
   lhOrdCustomer = BUFFER OrderCustomer:HANDLE.
   RUN StarEventInitialize(lhOrdCustomer).
END.    

Func.Common:mTS2Date(FusionMessage.CreatedTS, OUTPUT ldaOrigCancelDate).
   
/* Parsing address data to JSON */
ASSIGN
   lcHost        = fCParam("Masmovil", "Host")   
   liPort        = fIParam("Masmovil", "Port")     
   lcUriPath     = fCParam("Masmovil", "UriPath")   
   lcUriQuery    = fCParam("Masmovil", "UriQuery")  
   lcUriQueryVal = fCParam("Masmovil", "UriQueryValue")
   liLogRequest  = fIParam("Masmovil", "LogRequest")
   llLogRequest  = LOGICAL(liLogRequest).

lobjOuterObject = NEW JsonObject().
lobjOuterObject:ADD('orderType','ChangeInstallationAddress').
lobjOuterObject:ADD('orderID',liOrderId).
lobjOuterObject:ADD('createdBy','YOIGO').
   
lcServicesArray = NEW JsonArray().
lobjOuterObject:ADD('Services',lcServicesArray).
lcServicesObject = NEW JsonObject().
lcServicesArray:ADD(lcServicesObject).
    
lcCharacteristicsArray = NEW JsonArray().
lcCharacteristicsObject1 = NEW JsonObject().
lcCharacteristicsObject2 = NEW JsonObject().
lcCharacteristicsObject3 = NEW JsonObject().
 
lcServicesObject:ADD('Characteristics',lcCharacteristicsArray).
lcCharacteristicsArray:ADD(lcCharacteristicsObject1).
lcCharacteristicsObject1:ADD('name','AddressId').
lcCharacteristicsObject1:ADD('value',ENTRY(13,lcAmendamentValue,"||")).
  
lcCharacteristicsArray:ADD(lcCharacteristicsObject2).
lcCharacteristicsObject2:ADD('name','TerritoryOwner').
lcCharacteristicsObject2:ADD('value',ENTRY(18,lcAmendamentValue,"||")).

lcCharacteristicsArray:ADD(lcCharacteristicsObject3).
lcCharacteristicsObject3:ADD('name','gescal').
lcCharacteristicsObject3:ADD('value',ENTRY(12,lcAmendamentValue,"||")).
  
lcServicesObject:ADD('type','FTTH').
   
lcInstallationObject = NEW JsonObject().
lobjOuterObject:ADD('Installation',lcInstallationObject).
lcAddressObject = NEW JsonObject().
lcInstallationObject:ADD('Address',lcAddressObject).
  
lcAddressObject:ADD('country',ENTRY(14,lcAmendamentValue,"||")).
lcAddressObject:ADD('door',ENTRY(5,lcAmendamentValue,"||")).
lcAddressObject:ADD('zipCode',ENTRY(10,lcAmendamentValue,"||")).
lcAddressObject:ADD('streetType',ENTRY(1,lcAmendamentValue,"||")).
lcAddressObject:ADD('km',ENTRY(15,lcAmendamentValue,"||")).
lcAddressObject:ADD('stair',ENTRY(7,lcAmendamentValue,"||")).
lcAddressObject:ADD('town',ENTRY(11,lcAmendamentValue,"||")).
lcAddressObject:ADD('number',ENTRY(3,lcAmendamentValue,"||")).
lcAddressObject:ADD('province',ENTRY(15,lcAmendamentValue,"||")).
lcAddressObject:ADD('street',ENTRY(2,lcAmendamentValue,"||")).
lcAddressObject:ADD('letter',ENTRY(6,lcAmendamentValue,"||")).
lcAddressObject:ADD('bis',ENTRY(9,lcAmendamentValue,"||")).
lcAddressObject:ADD('block',ENTRY(8,lcAmendamentValue,"||")).
lcAddressObject:ADD('floor',ENTRY(4,lcAmendamentValue,"||")).
lcAddressObject:ADD('hand',ENTRY(16,lcAmendamentValue,"||")).
/*    
RUN Gwy/http_rest_client.p("put"     ,
                           lcHost    ,
                           liPort    ,     
                           ""        ,
                           ""        ,
                           lcUriPath ,
                           lcUriQuery,
                           lcUriQueryVal,
                           lobjOuterObject,
                           OUTPUT loJson).
                              
IF VALID-OBJECT(loJson) THEN
    ASSIGN  
        lcResultCode = loJson:GetCharacter("codigo")
        lcResultDesc = loJson:GetCharacter("_") NO-ERROR.*/
        
assign lcresultcode = "00".

IF lcResultCode NE "00" THEN DO:
   ASSIGN
      FusionMessage.UpdateTS = Func.Common:mMakeTS()
      FusionMessage.MessageStatus = {&FUSIONMESSAGE_STATUS_ERROR}
      FusionMessage.ResponseCode = (IF lcResultCode > ""
                                  THEN lcResultCode
                                  ELSE "ERROR")
      FusionMessage.AdditionalInfo = (IF lcResultDesc > ""
                                      THEN lcResultDesc
                                      ELSE "ERROR").

   IF fCanRetryFusionMessage(
      BUFFER FusionMessage,
      "",
      lcResultCode,
      lcResultDesc) THEN 
      RETURN "RETRY:" +
         SUBST("&1, &2, &3", lcError, lcResultCode, lcResultDesc).

   ASSIGN
      OrderFusion.UpdateTS = FusionMessage.UpdateTS
      OrderFusion.FusionStatus = {&FUSION_ORDER_STATUS_ERROR}
      OrderFusion.FusionStatusDesc = "update order failed".

   Func.Common:mWriteMemoWithType("Order",
                                  STRING(Order.OrderId),
                                  0,
                                  "Masmovil order updation failed",
                                  SUBST("ErrorCode: &1", (IF lcResultDesc > ""
                                                          THEN lcResultDesc 
                                                          ELSE "ERROR")),
                                  "",
                                  "TMS").

   fReqStatus(3,lcResultDesc).
   RETURN SUBST("&1, &2, &3", "ERROR", lcResultCode, lcResultDesc). 
END.

ELSE DO:
   /* update installation address */
   ASSIGN
      OrderFusion.FusionStatus = {&FUSION_ORDER_STATUS_INITIALIZED}
      OrderFusion.UpdateTS = Func.Common:mMakeTS()
      FusionMessage.UpdateTS = OrderFusion.UpdateTS
      FusionMessage.MessageStatus = {&FUSIONMESSAGE_STATUS_HANDLED}
      FusionMessage.ResponseCode = lcResultCode 
      FusionMessage.AdditionalInfo = lcResultDesc.
      
   ASSIGN 
      lcTableName     = "install_address_change"
      lcActionID      = "install_address_change_processor" 
      ldCurrentTimeTS = Func.Common:mMakeTS(). 

   FIND FIRST ActionLog EXCLUSIVE-LOCK WHERE
              ActionLog.Brand     EQ  Syst.Var:gcBrand AND
              ActionLog.ActionID  EQ  lcActionID       AND
              ActionLog.TableName EQ  lcTableName      NO-ERROR.
   IF NOT AVAIL ActionLog THEN DO:
   /*First execution stamp*/
      CREATE ActionLog.
      ASSIGN
         ActionLog.Brand        = Syst.Var:gcBrand
         ActionLog.TableName    = lcTableName
         ActionLog.ActionID     = lcActionID
         ActionLog.ActionStatus = {&ACTIONLOG_STATUS_PROCESSING}
         ActionLog.UserCode     = Syst.Var:katun
         ActionLog.ActionTS     = ldCurrentTimeTS.
   END.
   ELSE DO:
      ASSIGN
         ActionLog.ActionStatus = {&ACTIONLOG_STATUS_PROCESSING}
         ActionLog.UserCode     = Syst.Var:katun
         ActionLog.ActionTS     = ldCurrentTimeTS.
      RELEASE Actionlog.
   END.

   FIND FIRST OrderCustomer WHERE 
              OrderCustomer.Brand   EQ Syst.Var:gcBrand AND
              OrderCustomer.OrderId EQ FusionMessage.OrderID AND
              OrderCustomer.RowType EQ {&ORDERCUSTOMER_ROWTYPE_FIXED_INSTALL}
              EXCLUSIVE-LOCK NO-WAIT NO-ERROR.      
   IF NOT AVAIL OrderCustomer THEN 
      RETURN "ErrorCode: OrderCustomer is not available".
   ELSE DO:
       
      IF llDoEvent THEN RUN StarEventSetOldBuffer(lhOrdCustomer).
      ASSIGN
         OrderCustomer.AddressId    = ENTRY(13,lcAmendamentValue,"||")
         OrderCustomer.Gescal       = ENTRY(12,lcAmendamentValue,"||")
         OrderCustomer.Door         = ENTRY(5,lcAmendamentValue,"||")
         OrderCustomer.ZipCode      = ENTRY(10,lcAmendamentValue,"||")
         OrderCustomer.StreetType   = ENTRY(1,lcAmendamentValue,"||")
         OrderCustomer.Km           = ENTRY(15,lcAmendamentValue,"||")
         OrderCustomer.Stair        = ENTRY(7,lcAmendamentValue,"||")
         OrderCustomer.PostOffice   = ENTRY(11,lcAmendamentValue,"||")
         OrderCustomer.BuildingNum  = ENTRY(3,lcAmendamentValue,"||")
         OrderCustomer.Region       = ENTRY(15,lcAmendamentValue,"||")
         OrderCustomer.Street       = ENTRY(2,lcAmendamentValue,"||")
         OrderCustomer.Letter       = ENTRY(6,lcAmendamentValue,"||")
         OrderCustomer.BisDuplicate = ENTRY(9,lcAmendamentValue,"||")
         OrderCustomer.Block        = ENTRY(8,lcAmendamentValue,"||")
         OrderCustomer.Floor        = ENTRY(4,lcAmendamentValue,"||")
         OrderCustomer.Hand         = ENTRY(16,lcAmendamentValue,"||")
         OrderCustomer.Address      = OrderCustomer.Street.
         
      IF OrderCustomer.BuildingNum NE "" THEN 
         OrderCustomer.Address = OrderCustomer.Address + "||" +
            OrderCustomer.BuildingNum.
               
      IF llDoEvent THEN 
      RUN StarEventMakeModifyEventWithMemo(lhOrdCustomer, 
                                           {&STAR_EVENT_USER}, 
                                           lcMemo).    

      fReqStatus(6,"New AddressDetailes Updated to Order"). 
      fReqStatus(2,"Ok").
      
   END.      
   RELEASE OrderCustomer.
END.

CATCH e AS Progress.Lang.Error:
    ASSIGN lcResultDesc = e:GetMessage(1). 
    RETURN SUBST("ErrorCode: &1 ", (IF lcResultDesc > "" THEN lcResultDesc ELSE '')).
END CATCH.
                             
FINALLY:
      FIND FIRST ActionLog WHERE
                 ActionLog.Brand        EQ  Syst.Var:gcBrand  AND
                 ActionLog.ActionID     EQ  lcActionID        AND
                 ActionLog.TableName    EQ  lcTableName       AND
                 ActionLog.ActionStatus NE  {&ACTIONLOG_STATUS_SUCCESS}
                 EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
      IF AVAIL ActionLog THEN 
         ActionLog.ActionStatus = {&ACTIONLOG_STATUS_SUCCESS}.
      RELEASE ActionLog.
END FINALLY.
