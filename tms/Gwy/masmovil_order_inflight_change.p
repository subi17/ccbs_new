ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Progress.Json.ObjectModel.*.
USING OpenEdge.Net.HTTP.MethodEnum.

{Syst/tmsconst.i}
{Func/log.i}
{Func/masmovileif.i}
{Mc/orderfusion.i}
{Syst/eventval.i}
{Func/msreqfunc.i}
{Func/cparam2.i}

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER Syst.Var:katun
   {Func/lib/eventlog.i}
   DEF VAR lhOrdCustomer            AS HANDLE     NO-UNDO.
   DEF VAR lhOrdFusion              AS HANDLE     NO-UNDO.
   lhOrdCustomer = BUFFER OrderCustomer:HANDLE.
   lhOrdFusion   = BUFFER OrderFUsion:HANDLE .
   RUN StarEventInitialize(lhOrdCustomer).
END.  

DEF INPUT PARAM piMessageSeq     AS INT        NO-UNDO.

/* Parsing Order data to JSON */
DEF VAR loReqJson                AS JsonObject NO-UNDO.
DEF VAR loInstObject             AS JsonObject NO-UNDO.
DEF VAR lcServicesArray          AS JsonArray  NO-UNDO.
DEF VAR lcServicesObject         AS JsonObject NO-UNDO.
DEF VAR lcCharacteristicsArray   AS JsonArray  NO-UNDO.
DEF VAR lcCharacteristicsObject1 AS JsonObject NO-UNDO.
DEF VAR lcCharacteristicsObject2 AS JsonObject NO-UNDO.
DEF VAR lcCharacteristicsObject3 AS JsonObject NO-UNDO.
DEF VAR lcInstallationObject     AS JsonObject NO-UNDO.
DEF VAR lcAddressObject          AS JsonObject NO-UNDO.
DEF VAR loJson                   AS JsonObject NO-UNDO.

DEF VAR loFixObject              AS JsonObject NO-UNDO.
DEF VAR lcFixServicesArray          AS JsonArray  NO-UNDO.
DEF VAR lcFixServicesObject         AS JsonObject NO-UNDO.
DEF VAR lcFixCharacteristicsArray   AS JsonArray  NO-UNDO.
DEF VAR lcFixCharacteristicsObject  AS JsonObject NO-UNDO.

DEF VAR lcHost                   AS CHARACTER  NO-UNDO.
DEF VAR liPort                   AS INTEGER    NO-UNDO.
DEF VAR lcUriPath                AS CHARACTER  NO-UNDO.
DEF VAR lcUriQuery               AS CHARACTER  NO-UNDO.
DEF VAR lcUriQueryValue          AS CHARACTER  NO-UNDO.
DEF VAR lcHeaderName             AS CHAR       NO-UNDO.
DEF VAR lcHeaderValue            AS CHAR       NO-UNDO.
DEF VAR liLogRequest             AS INTEGER    NO-UNDO.
DEF VAR llLogRequest             AS LOGICAL    NO-UNDO INIT TRUE.
DEF VAR oiStatusCode             AS INTEGER    NO-UNDO. 
DEF VAR ocStatusReason           AS CHARACTER  NO-UNDO. 
DEF VAR loResponseJson           AS JsonObject NO-UNDO.
   
DEF VAR lcAmendamentValue        AS CHAR       NO-UNDO.
DEF VAR lcOrderId                AS CHAR       NO-UNDO.

DEF BUFFER FusionMessage FOR FusionMessage.
DEF BUFFER Order         FOR Order.
DEF BUFFER OrderFusion   FOR OrderFusion.
DEF BUFFER MSRequest     FOR MSRequest.
DEF BUFFER OrderCustomer FOR OrderCustomer.

/* Update Installation Address */
      
FIND FusionMessage EXCLUSIVE-LOCK WHERE
     FusionMessage.MessageSeq = piMessageSeq NO-WAIT NO-ERROR.
     
IF FusionMessage.MessageType NE {&FUSIONMESSAGE_TYPE_ADDRESS_CHANGE} AND  
   FusionMessage.MessageType NE {&FUSIONMESSAGE_TYPE_PHONE_NUMBER_CHANGE} THEN
   RETURN SUBST("Incorrect message type: &1", FusionMessage.MessageType).
   
FIND Order EXCLUSIVE-LOCK WHERE
     Order.Brand = Syst.Var:gcBrand AND
     Order.OrderId = FusionMessage.OrderID 
     NO-WAIT NO-ERROR.

FIND OrderFusion EXCLUSIVE-LOCK WHERE
     OrderFusion.Brand = Syst.Var:gcBrand AND
     OrderFusion.OrderID = FusionMessage.OrderID
     NO-WAIT NO-ERROR.

IF OrderFusion.FixedNumber EQ "" THEN
   RETURN fFusionMessageError(BUFFER FusionMessage,
                             "FixedNumber not assigned").

FIND FIRST MSRequest EXCLUSIVE-LOCK USE-INDEX updatestamp WHERE 
           MSRequest.MsSeq      = FusionMessage.Msseq AND
           MSRequest.ReqType    = {&REQTYPE_FIXEDLINE_ORDER_UPDATE} AND
           MSRequest.ReqCParam2 = {&INFLIGHT_ADDRESS_UPDATE}  OR 
           MSRequest.ReqCParam2 = {&INFLIGHT_PHONE_NUMBER_UPDATE}
           NO-WAIT NO-ERROR.

IF AVAIL MSRequest THEN 
   ASSIGN 
      lcAmendamentValue = MSRequest.ReqCParam3
      lcOrderId = SUBST("Y&1", Order.OrderID).

/* Parsing address data to JSON */
ASSIGN
   lcHost          = fCParam("Masmovil", "InflightHost")   
   liPort          = fIParam("Masmovil", "InflightPort")     
   lcUriPath       = fCParam("Masmovil", "InflightUriPath")   
   lcUriQuery      = fCParam("Masmovil", "InflightUriQuery")
   lcUriQueryValue = fCParam("Masmovil", "InflightUriQueryValue")
   lcHeaderName    = fCParam("Masmovil", "InflightHeaderName")
   lcHeaderValue   = fCParam("Masmovil", "InflightHeaderValue")
   liLogRequest    = fIParam("Masmovil", "InflightLogRequest")
   llLogRequest    = LOGICAL(liLogRequest)
   lcUriPath       = lcUriPath + SUBST("Y&1", Order.OrderID)
   loReqJson       = NEW JsonObject().

CASE MSRequest.ReqCParam2:
   WHEN {&INFLIGHT_ADDRESS_UPDATE} THEN DO:
    	
      loInstObject = NEW JsonObject().
      loInstObject:ADD('orderType','ChangeInstallationAddress').
      loInstObject:ADD('orderID',lcOrderId).
      loInstObject:ADD('createdBy','YOIGO').
   
      lcServicesArray = NEW JsonArray().
      loInstObject:ADD('Services',lcServicesArray).
      lcServicesObject = NEW JsonObject().
      lcServicesArray:ADD(lcServicesObject).
          
      lcCharacteristicsArray  = NEW JsonArray().
      lcCharacteristicsObject1 = NEW JsonObject().
      lcCharacteristicsObject2 = NEW JsonObject().
      lcCharacteristicsObject3 = NEW JsonObject().
        
      lcServicesObject:ADD('Characteristics',lcCharacteristicsArray).
      lcCharacteristicsArray:ADD(lcCharacteristicsObject1).
      lcCharacteristicsObject1:ADD('name','AddressId').
      lcCharacteristicsObject1:ADD('value',ENTRY(13,lcAmendamentValue,"|")).
         
      lcCharacteristicsArray:ADD(lcCharacteristicsObject2).
      lcCharacteristicsObject2:ADD('name','TerritoryOwner').
      lcCharacteristicsObject2:ADD('value',ENTRY(18,lcAmendamentValue,"|")).
       
      lcCharacteristicsArray:ADD(lcCharacteristicsObject3).
      lcCharacteristicsObject3:ADD('name','gescal').
      lcCharacteristicsObject3:ADD('value',ENTRY(12,lcAmendamentValue,"|")).
         
      lcServicesObject:ADD('type','FTTH').
          
      lcInstallationObject = NEW JsonObject().
      loInstObject:ADD('Installation',lcInstallationObject).
      lcAddressObject = NEW JsonObject().
      lcInstallationObject:ADD('Address',lcAddressObject).
         
      lcAddressObject:ADD('country',ENTRY(14,lcAmendamentValue,"|")).
      lcAddressObject:ADD('door',ENTRY(5,lcAmendamentValue,"|")).
      lcAddressObject:ADD('zipCode',ENTRY(10,lcAmendamentValue,"|")).
      lcAddressObject:ADD('streetType',ENTRY(1,lcAmendamentValue,"|")).
      lcAddressObject:ADD('km',ENTRY(15,lcAmendamentValue,"|")).
      lcAddressObject:ADD('stair',ENTRY(7,lcAmendamentValue,"|")).
      lcAddressObject:ADD('town',ENTRY(11,lcAmendamentValue,"|")).
      lcAddressObject:ADD('number',ENTRY(3,lcAmendamentValue,"|")).
      lcAddressObject:ADD('province',ENTRY(16,lcAmendamentValue,"|")).
      lcAddressObject:ADD('street',ENTRY(2,lcAmendamentValue,"|")).
      lcAddressObject:ADD('letter',ENTRY(6,lcAmendamentValue,"|")).
      lcAddressObject:ADD('bis_duplicate',ENTRY(9,lcAmendamentValue,"|")).
      lcAddressObject:ADD('block',ENTRY(8,lcAmendamentValue,"|")).
      lcAddressObject:ADD('floor',ENTRY(4,lcAmendamentValue,"|")).
      lcAddressObject:ADD('hand',ENTRY(17,lcAmendamentValue,"|")).
      
      Assign loReqJson = loInstObject.
   END.
       
   WHEN {&INFLIGHT_PHONE_NUMBER_UPDATE} THEN DO:
    	
      loFixObject = NEW JsonObject().
      loFixObject:ADD('orderType','ChangePhoneNumber').
      loFixObject:ADD('orderID',lcOrderId).
      loFixObject:ADD('createdBy','YOIGO').
       
      lcFixServicesArray = NEW JsonArray().
      loFixObject:ADD('Services',lcFixServicesArray).
      lcFixServicesObject = NEW JsonObject().
      lcFixServicesArray:ADD(lcFixServicesObject).
       
      lcFixCharacteristicsArray = NEW JsonArray().
      lcFixCharacteristicsObject = NEW JsonObject().
       
      lcFixServicesObject:ADD('Characteristics',lcFixCharacteristicsArray).
      lcFixCharacteristicsArray:ADD(lcFixCharacteristicsObject).
      lcFixCharacteristicsObject:ADD('name','phoneNumber').
      lcfixCharacteristicsObject:ADD('value',lcAmendamentValue).
      
      lcfixServicesObject:ADD('type','PHONE').
      Assign loReqJson = loFixObject.
   END. 
      
   OTHERWISE RETURN.
     
END CASE.
       
RUN Gwy/http_rest_client.p(STRING(OpenEdge.Net.HTTP.MethodEnum:PATCH),
                           lcHost           ,
                           liPort           ,
                           ""               , /* Authorization type */
                           ""               , /* Realm*/
                           ""               , /* user id */
                           ""               , /* password */
                           lcUriPath        ,
                           lcUriQuery       ,
                           lcUriQueryValue  ,
                           lcHeaderName     ,
                           lcHeaderValue    ,
                           loReqJson        ,
                           OUTPUT oiStatusCode,
                           OUTPUT ocStatusReason,
                           OUTPUT loResponseJson).

        
IF oiStatusCode NE 200 THEN DO:                           
   ASSIGN
      FusionMessage.UpdateTS       = Func.Common:mMakeTS()
      FusionMessage.MessageStatus  = {&FUSIONMESSAGE_STATUS_ERROR}
      FusionMessage.ResponseCode   = STRING(oiStatusCode)
      FusionMessage.AdditionalInfo = ocStatusReason.

   IF fCanRetryFusionMessage(BUFFER FusionMessage,
                             "",
                             STRING(oiStatusCode),
                             ocStatusReason) THEN 
      RETURN "RETRY:" + SUBST("&1, &2", oiStatusCode, ocStatusReason).

   Func.Common:mWriteMemoWithType("OrderCustomer",
                                  STRING(Order.OrderId),
                                  0,
                                  "Installation order updation failed",
                                  SUBST("ErrorCode: &1", (IF ocStatusReason > "" THEN
                                                             ocStatusReason 
                                                          ELSE "ERROR")),
                                  "",
                                  "TMS").

   fReqStatus(3,ocStatusReason).
END.
ELSE DO: 
   /* update Order */
   ASSIGN
      FusionMessage.UpdateTS       = Func.Common:mMakeTS()
      FusionMessage.MessageStatus  = {&FUSIONMESSAGE_STATUS_HANDLED}
      FusionMessage.ResponseCode   = STRING(oiStatusCode) 
      FusionMessage.AdditionalInfo = ocStatusReason.
      
   CASE MSRequest.ReqCParam2:
      WHEN {&INFLIGHT_ADDRESS_UPDATE} THEN DO: 
         RUN pUpdateInflightOrderDetails({&INFLIGHT_ADDRESS_UPDATE},FusionMessage.OrderID, {&ORDERCUSTOMER_ROWTYPE_FIXED_INSTALL}).
         RUN pUpdateInflightOrderDetails({&INFLIGHT_ADDRESS_UPDATE},FusionMessage.OrderID, {&ORDERCUSTOMER_ROWTYPE_AGREEMENT}).  
      END.   
      WHEN {&INFLIGHT_PHONE_NUMBER_UPDATE} THEN 
      	RUN pUpdateInflightOrderDetails({&INFLIGHT_PHONE_NUMBER_UPDATE},FusionMessage.OrderID, {&ORDERCUSTOMER_ROWTYPE_FIXED_INSTALL}).
         
      OTHERWISE RETURN.
   END CASE.

END.

RETURN "".

FINALLY:
   IF llDoEvent THEN fCleanEventObjects().
END.

PROCEDURE pUpdateInflightOrderDetails:
   
   DEF INPUT PARAMETER icAmendmentType AS CHAR NO-UNDO.   
   DEF INPUT PARAMETER iiOrderID       AS INT NO-UNDO.
   DEF INPUT PARAMETER iiRowType       AS INT NO-UNDO. 
   
   FIND FIRST OrderCustomer EXCLUSIVE-LOCK WHERE 
              OrderCustomer.Brand   EQ Syst.Var:gcBrand AND
              OrderCustomer.OrderId EQ iiOrderID        AND
              OrderCustomer.RowType EQ iiRowType 
              NO-WAIT NO-ERROR.      
   IF NOT AVAIL OrderCustomer THEN 
      RETURN "ErrorCode: OrderCustomer is not available".
    
   IF llDoEvent THEN RUN StarEventSetOldBuffer(lhOrdCustomer).
   
   CASE icAmendmentType:
      WHEN {&INFLIGHT_ADDRESS_UPDATE} THEN DO: 
        
         ASSIGN
            OrderCustomer.StreetType   = ENTRY(1,lcAmendamentValue,"|")
            OrderCustomer.Street       = ENTRY(2,lcAmendamentValue,"|")
            OrderCustomer.BuildingNum  = ENTRY(3,lcAmendamentValue,"|")
            OrderCustomer.Floor        = ENTRY(4,lcAmendamentValue,"|")
            OrderCustomer.Door         = ENTRY(5,lcAmendamentValue,"|")
            OrderCustomer.Letter       = ENTRY(6,lcAmendamentValue,"|")
            OrderCustomer.Stair        = ENTRY(7,lcAmendamentValue,"|")
            OrderCustomer.Block        = ENTRY(8,lcAmendamentValue,"|")
            OrderCustomer.BisDuplicate = ENTRY(9,lcAmendamentValue,"|")
            OrderCustomer.ZipCode      = ENTRY(10,lcAmendamentValue,"|")
            OrderCustomer.PostOffice   = ENTRY(11,lcAmendamentValue,"|")
            OrderCustomer.Gescal       = ENTRY(12,lcAmendamentValue,"|")
            OrderCustomer.AddressId    = ENTRY(13,lcAmendamentValue,"|")
            OrderCustomer.Country      = ENTRY(14,lcAmendamentValue,"|")
            OrderCustomer.Km           = ENTRY(15,lcAmendamentValue,"|")
            OrderCustomer.Region       = ENTRY(16,lcAmendamentValue,"|")
            OrderCustomer.Hand         = ENTRY(17,lcAmendamentValue,"|")
            OrderCustomer.Address      = OrderCustomer.Street.
         
         IF OrderCustomer.BuildingNum NE "" THEN 
            OrderCustomer.Address = OrderCustomer.Address + "|" +
                                     OrderCustomer.BuildingNum.
         
         IF llDoEvent THEN 
            RUN StarEventMakeModifyEventWithMemo(lhOrdCustomer, 
                                                 {&STAR_EVENT_USER}, 
                                                 "Installation Address Updated").    
         
      END.
         
      WHEN {&INFLIGHT_PHONE_NUMBER_UPDATE} THEN DO:
         
         ASSIGN
            OrderFusion.FixedNumber     = lcAmendamentValue 
            OrderFusion.FixedNumberType = "NEW" .
         
            IF llDoEvent THEN DO:
               RUN StarEventMakeModifyEventWithMemo(lhOrdCustomer,
                                                   {&STAR_EVENT_USER},
                                                   "Fixed Number Updated").
               RUN StarEventMakeModifyEventWithMemo(lhOrdFusion,
                                                    {&STAR_EVENT_USER},
                                                    "Fixed Number and Type Updated").
            END.
      END.     
         
      OTHERWISE RETURN.
   END CASE.
               
   RELEASE OrderCustomer.
   fReqStatus(2,""). 
END PROCEDURE.
