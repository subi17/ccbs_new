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
   lhOrdCustomer = BUFFER OrderCustomer:HANDLE.
   RUN StarEventInitialize(lhOrdCustomer).
END.

DEF INPUT PARAM piMessageSeq     AS INT        NO-UNDO.

/* Parsing FixedNumber data to JSON */
DEF VAR loInstObject             AS JsonObject NO-UNDO.
DEF VAR lcServicesArray          AS JsonArray  NO-UNDO.
DEF VAR lcServicesObject         AS JsonObject NO-UNDO.
DEF VAR lcCharacteristicsArray   AS JsonArray  NO-UNDO.
DEF VAR lcCharacteristicsObject  AS JsonObject NO-UNDO.

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

/* Update fixed number */
FIND FusionMessage EXCLUSIVE-LOCK WHERE
     FusionMessage.MessageSeq = piMessageSeq NO-WAIT NO-ERROR.

IF FusionMessage.MessageType NE {&FUSIONMESSAGE_TYPE_PHONE_NUMBER_CHANGE} THEN
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
           MSRequest.ReqCParam2 = "ChangePhoneNumber"
           NO-WAIT NO-ERROR.

IF AVAIL MSRequest THEN
   ASSIGN
      lcAmendamentValue = MSRequest.ReqCParam3
      lcOrderId = SUBST("Y&1", Order.OrderID).

/* Parsing FixedNumber data to JSON */
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
   lcUriPath       = lcUriPath + SUBST("Y&1", Order.OrderID).

loInstObject = NEW JsonObject().
loInstObject:ADD('orderType','ChangePhoneNumber').
loInstObject:ADD('orderID',lcOrderId).
loInstObject:ADD('createdBy','YOIGO').

lcServicesArray = NEW JsonArray().
loInstObject:ADD('Services',lcServicesArray).
lcServicesObject = NEW JsonObject().
lcServicesArray:ADD(lcServicesObject).

lcCharacteristicsArray = NEW JsonArray().
lcCharacteristicsObject = NEW JsonObject().

lcServicesObject:ADD('Characteristics',lcCharacteristicsArray).
lcCharacteristicsArray:ADD(lcCharacteristicsObject).
lcCharacteristicsObject:ADD('name','phoneNumber').
lcCharacteristicsObject:ADD('value','lcAmendamentValue').

lcServicesObject:ADD('type','PHONE').
DEF STREAM strfixchng.
output stream strfixchng to value("/tmp/fixnum_mmfixchg" + STRING(TODAY, "99999999") + ".txt") append.
         export stream strfixchng REPLACE(STRING(TIME, "HH:MM:SS"),":","") lcAmendamentValue STRING(OpenEdge.Net.HTTP.MethodEnum:PATCH) lcHost liPort lcUriPath lcUriQuery lcUriQueryValue lcHeaderName lcHeaderValue.
         output stream strfixchng  close.

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
                           loInstObject     ,
                           OUTPUT oiStatusCode,
                           OUTPUT ocStatusReason,
                           OUTPUT loResponseJson).

DEF STREAM strfixchng1.
output stream strfixchng1 to value("/tmp/fixnum_mmfixchg1" + STRING(TODAY, "99999999") + ".txt") append.
         export stream strfixchng1 REPLACE(STRING(TIME, "HH:MM:SS"),":","") oiStatusCode ocStatusReason.
         output stream strfixchng1  close.

IF oiStatusCode NE 200 THEN DO:
   ASSIGN
      FusionMessage.UpdateTS      = Func.Common:mMakeTS()
      FusionMessage.MessageStatus = {&FUSIONMESSAGE_STATUS_ERROR}
      FusionMessage.ResponseCode  = (IF ocStatusReason > "" THEN
                                         ocStatusReason
                                      ELSE "ERROR").

   IF fCanRetryFusionMessage(
      BUFFER FusionMessage,
      "",
      STRING(oiStatusCode),
      ocStatusReason) THEN
      RETURN "RETRY:" +
         SUBST("&1, &2", oiStatusCode, ocStatusReason).

   Func.Common:mWriteMemoWithType("OrderCustomer",
                                  STRING(Order.OrderId),
                                  0,
                                  "Fixed Number updation failed",
                                  SUBST("ErrorCode: &1", (IF ocStatusReason > "" THEN
                                                             ocStatusReason
                                                          ELSE "ERROR")),
                                  "",
                                  "TMS").

   fReqStatus(3,ocStatusReason).
END.
ELSE DO:
    /* update fixed number */

   ASSIGN
      FusionMessage.UpdateTS       = Func.Common:mMakeTS()
      FusionMessage.MessageStatus  = {&FUSIONMESSAGE_STATUS_HANDLED}
      FusionMessage.ResponseCode   = STRING(oiStatusCode)
      FusionMessage.AdditionalInfo = ocStatusReason.

   FIND FIRST OrderCustomer WHERE
              OrderCustomer.Brand   EQ Syst.Var:gcBrand AND
              OrderCustomer.OrderId EQ FusionMessage.OrderID AND
              OrderCustomer.RowType EQ {&ORDERCUSTOMER_ROWTYPE_FIXED_INSTALL}
              EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
   IF NOT AVAIL OrderCustomer THEN
      RETURN "ErrorCode: OrderCustomer is not available".

   IF llDoEvent THEN RUN StarEventSetOldBuffer(lhOrdCustomer).

   ASSIGN
      OrderCustomer.FixedNumber   = lcAmendamentValue.

   IF llDoEvent THEN
      RUN StarEventMakeModifyEventWithMemo(lhOrdCustomer,
                                           {&STAR_EVENT_USER},
                                           "Fixed Number Updated").

      RELEASE OrderCustomer.
      fReqStatus(2,"").

END.

RETURN "".
