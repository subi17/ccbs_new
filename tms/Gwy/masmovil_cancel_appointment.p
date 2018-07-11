USING Progress.Json.ObjectModel.*.

{Syst/tmsconst.i}
{Func/log.i}
{Func/cparam2.i}
{Func/masmovileif.i}
{Mc/orderfusion.i}

DEF INPUT PARAM piMessageSeq AS INT NO-UNDO.

DEF BUFFER bFusionMessage FOR FusionMessage.

DEF VAR liMaxRetry     AS INTEGER    NO-UNDO INIT 0.
DEF VAR liCount        AS INTEGER    NO-UNDO INIT 0.
DEF VAR lcHost         AS CHARACTER  NO-UNDO.
DEF VAR liPort         AS INTEGER    NO-UNDO.
DEF VAR lcAuthType     AS CHARACTER  NO-UNDO.
DEF VAR lcRealm        AS CHARACTER  NO-UNDO.
DEF VAR lcUserId       AS CHARACTER  NO-UNDO.
DEF VAR lcPassword     AS CHARACTER  NO-UNDO.
DEF VAR lcUriPath      AS CHARACTER  NO-UNDO.
DEF VAR lcUriQuery     AS CHARACTER  NO-UNDO.
DEF VAR lcUriQueryVal  AS CHARACTER  NO-UNDO.
DEF VAR liLogRequest   AS INTEGER    NO-UNDO.
DEF VAR llLogRequest   AS LOGICAL    NO-UNDO INIT TRUE.
DEF VAR oiStatusCode   AS INTEGER    NO-UNDO. 
DEF VAR ocStatusReason AS CHARACTER  NO-UNDO. 
DEF VAR loRequestJson  AS JsonObject NO-UNDO.
DEF VAR loResponseJson AS JsonObject NO-UNDO.

FUNCTION fLogRequest RETURNS CHAR
   (iiOrderId AS INT,
    ioJson    AS JsonObject):

   DEF VAR llcJson AS LONGCHAR NO-UNDO.

   IF llLogRequest AND VALID-OBJECT(ioJson) THEN
   DO:
      ioJson:WRITE( llcJson,TRUE).
      OUTPUT STREAM sOut TO VALUE("/tmp/Xmasmovile_" + STRING(iiOrderId) + "_cancel_appointment_" + REPLACE(STRING(Func.Common:mMakeTS()), ".", "_") + ".json") APPEND.
      PUT STREAM sOut UNFORMATTED string(llcJson) SKIP.
      OUTPUT STREAM sOut CLOSE.
   END.

   RETURN "".

END FUNCTION.

FIND FusionMessage EXCLUSIVE-LOCK WHERE
     FusionMessage.MessageSeq EQ piMessageSeq NO-ERROR.
          
IF FusionMessage.MessageType NE {&FUSIONMESSAGE_TYPE_CANCEL_APPOINTMENT} THEN
   RETURN SUBST("Incorrect message type: &1", FusionMessage.MessageType).

FIND Order NO-LOCK WHERE
     Order.Brand   EQ Syst.Var:gcBrand      AND
     Order.OrderId EQ FusionMessage.OrderID NO-ERROR.

IF NOT AVAIL Order THEN 
   RETURN SUBST("OrderId is not available: &1", FusionMessage.OrderId).

ASSIGN
   lcHost        = fCParam("Masmovil","AppointmentHost")
   liport        = fIparam("Masmovil","AppointmentPort")
   lcAuthType    = fCParam("Masmovil","AppointmentAuthType")
   lcRealm       = fCParam("Masmovil","AppointmentRealm")
   lcUserId      = fCParam("Masmovil","AppointmentUserId")
   lcPassword    = fCParam("Masmovil","AppointmentPass")
   lcUriPath     = fCParam("Masmovil","AppointmentUriPath")
   lcUriQuery    = ""
   lcUriQueryVal = ""
   liLogRequest  = fIParam("Masmovil","AppointmentLogRequest")
   llLogRequest  = LOGICAL(liLogRequest)
   lcUriPath     = lcUriPath + SUBST("Y&1", Order.OrderID).

loRequestJson = NEW Progress.Json.ObjectModel.JsonObject().
loRequestJson:ADD("status" , "CANCEL").
loRequestJson:ADD("externalId" , SUBST("Y&1", Order.OrderID)). 

fLogRequest(Order.OrderId, loRequestJson).

RUN Gwy/http_rest_client.p(STRING(OpenEdge.Net.HTTP.MethodEnum:PATCH),
                           lcHost,
                           liPort,
                           lcAuthType,
                           lcRealm,
                           lcUserId,
                           lcPassword,
                           lcUriPath,
                           lcUriQuery,
                           lcUriQueryVal,
                           loRequestJson,
                           OUTPUT oiStatusCode,
                           OUTPUT ocStatusReason,
                           OUTPUT loResponseJson). 

IF oiStatusCode NE 200 THEN DO:

   ASSIGN FusionMessage.UpdateTS       = Func.Common:mMakeTS()
          FusionMessage.MessageStatus  = {&FUSIONMESSAGE_STATUS_ERROR}
          FusionMessage.ResponseCode   = "ERROR " + STRING(oiStatusCode)
          FusionMessage.AdditionalInfo = ocStatusReason 
          liMaxRetry                   = fIParam("Masmovil","AppointmentRetryMax").

   FOR EACH bFusionMessage NO-LOCK WHERE
            bFusionMessage.OrderID     EQ FusionMessage.OrderID     AND
            bFusionMessage.MessageType EQ FusionMessage.MessageType AND
            ROWID(bFusionMessage)      NE ROWID(FusionMessage):
      liCount = liCount + 1.
   END.

   IF liCount < liMaxRetry THEN 
      RETURN "RETRY".

END.
ELSE 
   ASSIGN FusionMessage.UpdateTS       = Func.Common:mMakeTS()
          FusionMessage.MessageStatus  = {&FUSIONMESSAGE_STATUS_HANDLED}
          FusionMessage.ResponseCode   = STRING(oiStatusCode)
          FusionMessage.AdditionalInfo = ocStatusReason.

RETURN "".

FINALLY:
   IF VALID-OBJECT(loRequestjson)
   THEN DELETE OBJECT loRequestjson.

   /* loResponseJson is not used */
   IF VALID-OBJECT(loResponseJson)
   THEN DELETE OBJECT loResponseJson.
END FINALLY.

