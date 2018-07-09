ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Progress.Json.ObjectModel.*.

{Syst/tmsconst.i}
{Func/log.i}
{Func/masmovileif.i}
{Mc/orderfusion.i}
{Syst/eventval.i}
{Func/msreqfunc.i}
{Func/cparam2.i}

&GLOBAL-DEFINE WAIT_CONFIG_LOCK 20
IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER Syst.Var:katun
   {Func/lib/eventlog.i}
   DEF VAR lhOrdCustomer            AS HANDLE     NO-UNDO.
   lhOrdCustomer = BUFFER OrderCustomer:HANDLE.
   RUN StarEventInitialize(lhOrdCustomer).
END.  

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
DEF VAR lcMemo                   AS CHAR       NO-UNDO.
DEF VAR lcError                  AS CHAR       NO-UNDO. 
DEF VAR ldaOrigCancelDate        AS DATE       NO-UNDO.

DEF VAR oiStatusCode             AS INT        NO-UNDO.
      
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

IF OrderFusion.FixedNumber EQ "" THEN
   RETURN fFusionMessageError(BUFFER FusionMessage,
                             "FixedNumber not assigned").

FIND FIRST MSRequest EXCLUSIVE-LOCK USE-INDEX updatestamp WHERE 
           MSRequest.MsSeq = FusionMessage.Msseq AND
           MSRequest.ReqType = {&REQTYPE_FIXEDLINE_ORDER_UPDATE} AND
           MSRequest.ReqCParam2 = "ChangeInstallationAddress" 
           NO-WAIT NO-ERROR.
IF AVAIL MSRequest THEN  
   lcAmendamentValue = MSRequest.ReqCParam3. 

Func.Common:mTS2Date(FusionMessage.CreatedTS, OUTPUT ldaOrigCancelDate).
   
/* Parsing address data to JSON */
ASSIGN
   lcHost        = fCParam("Masmovil", "InflightHost")   
   liPort        = fIParam("Masmovil", "InflightPort")     
   lcUriPath     = fCParam("Masmovil", "InflightUriPath")   
   lcUriQuery    = fCParam("Masmovil", "InflightUriQuery")  
   lcUriQueryVal = fCParam("Masmovil", "InflightUriQueryValue")
   liLogRequest  = fIParam("Masmovil", "InflightLogRequest")
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
lcCharacteristicsObject1:ADD('value',ENTRY(13,lcAmendamentValue,"|")).
  
lcCharacteristicsArray:ADD(lcCharacteristicsObject2).
lcCharacteristicsObject2:ADD('name','TerritoryOwner').
lcCharacteristicsObject2:ADD('value',ENTRY(18,lcAmendamentValue,"|")).

lcCharacteristicsArray:ADD(lcCharacteristicsObject3).
lcCharacteristicsObject3:ADD('name','gescal').
lcCharacteristicsObject3:ADD('value',ENTRY(12,lcAmendamentValue,"|")).
  
lcServicesObject:ADD('type','FTTH').
   
lcInstallationObject = NEW JsonObject().
lobjOuterObject:ADD('Installation',lcInstallationObject).
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
lcAddressObject:ADD('bis',ENTRY(9,lcAmendamentValue,"|")).
lcAddressObject:ADD('block',ENTRY(8,lcAmendamentValue,"|")).
lcAddressObject:ADD('floor',ENTRY(4,lcAmendamentValue,"|")).
lcAddressObject:ADD('hand',ENTRY(17,lcAmendamentValue,"|")).
/*
RUN Gwy/http_rest_client.p(STRING(MethodEnum:PUT),
                           lcHost    ,
                           liPort    ,     
                           ""        , /* authorization type */
                           ""        , /* realm or domain */
                           ""        ,
                           ""        ,
                           lcUriPath ,
                           lcUriQuery,
                           lcUriQueryVal,
                           loRequestJson,
                           OUTPUT oiStatusCode,
                           OUTPUT ocStatusReason,
                           OUTPUT loJson).
*/
Assign oiStatusCode = 200.
 
IF oiStatusCode EQ 200 THEN DO:
   
   /* update installation address */
   ASSIGN
      FusionMessage.UpdateTS = Func.Common:mMakeTS()
      FusionMessage.MessageStatus = {&FUSIONMESSAGE_STATUS_HANDLED}
      FusionMessage.ResponseCode = lcResultCode 
      FusionMessage.AdditionalInfo = lcResultDesc.
     
   FIND FIRST OrderCustomer WHERE 
              OrderCustomer.Brand   EQ Syst.Var:gcBrand AND
              OrderCustomer.OrderId EQ FusionMessage.OrderID AND
              OrderCustomer.RowType EQ {&ORDERCUSTOMER_ROWTYPE_FIXED_INSTALL}
              EXCLUSIVE-LOCK NO-WAIT NO-ERROR.      
   IF NOT AVAIL OrderCustomer THEN 
      RETURN "ErrorCode: OrderCustomer is not available".
    
   IF llDoEvent THEN RUN StarEventSetOldBuffer(lhOrdCustomer).
   
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
                                           lcMemo).    
      
      RELEASE OrderCustomer.
      fReqStatus(2,"").

   RETURN "". 
END.
ELSE DO:
   ASSIGN
      FusionMessage.UpdateTS = Func.Common:mMakeTS()
      FusionMessage.MessageStatus = {&FUSIONMESSAGE_STATUS_ERROR}
      FusionMessage.ResponseCode = (IF lcResultDesc > "" THEN 
                                         lcResultDesc
                                      ELSE "ERROR").

   IF fCanRetryFusionMessage(
      BUFFER FusionMessage,
      "",
      lcResultCode,
      lcResultDesc) THEN 
      RETURN "RETRY:" +
         SUBST("&1, &2, &3", lcError, lcResultCode, lcResultDesc).

   Func.Common:mWriteMemoWithType("Order",
                                  STRING(Order.OrderId),
                                  0,
                                  "Masmovil order updation failed",
                                  SUBST("ErrorCode: &1", (IF lcResultDesc > "" THEN
                                                             lcResultDesc 
                                                          ELSE "ERROR")),
                                  "",
                                  "TMS").

   fReqStatus(3,lcResultDesc).
END.
