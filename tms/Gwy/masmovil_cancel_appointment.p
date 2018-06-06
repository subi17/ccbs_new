USING Progress.Json.ObjectModel.*.

{Syst/tmsconst.i}
{Func/log.i}
{Func/cparam2.i}
{Func/masmovileif.i}
{Mc/orderfusion.i}

DEF INPUT PARAM piMessageSeq AS INT NO-UNDO.

DEF BUFFER bFusionMessage FOR FusionMessage.

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
DEF VAR loRequestJson  AS JsonObject NO-UNDO.
DEF VAR loResponseJson AS JsonObject NO-UNDO.

FUNCTION fLogRequest RETURNS CHAR
   (iiOrderId AS INT,
    ioJson    AS JsonObject):

   DEF VAR llcJson AS LONGCHAR NO-UNDO.

   IF llLogRequest AND VALID-OBJECT(ioJson) THEN
   DO:
      ioJson:WRITE( llcJson,TRUE).
      OUTPUT STREAM sOut TO VALUE("/tmp/Xmasmovile_" + string(iiOrderId) + "_cancel_appointment_" + REPLACE(STRING(Func.Common:mMakeTS()), ".", "_") + ".json") APPEND.
      PUT STREAM sOut UNFORMATTED string(llcJson) SKIP.
      OUTPUT STREAM sOut CLOSE.
   END.

   RETURN "".

END FUNCTION.

FIND FusionMessage EXCLUSIVE-LOCK WHERE
     FusionMessage.MessageSeq EQ piMessageSeq NO-ERROR.
          
IF FusionMessage.MessageType NE {&FUSIONMESSAGE_TYPE_CANCEL_APPOINTMENT} THEN
   RETURN SUBST("Incorrect message type: &1", FusionMessage.MessageType).

FIND Order EXCLUSIVE-LOCK WHERE
     Order.Brand   EQ Syst.Var:gcBrand      AND
     Order.OrderId EQ FusionMessage.OrderID NO-ERROR.

IF NOT AVAIL Order THEN 
   RETURN SUBST("OrderId is not available: &1", FusionMessage.OrderId).

ASSIGN
   lcHost        = fCParam("Masmovil","cancelAppointmentHost")
   liport        = fIparam("Masmovil","cancelAppointmentPort")
   lcAuthType    = fCParam("Masmovil","cancelAppointmnetAuthType")
   lcRealm       = fCParam("Masmovil","cancelAppointmentRealm")
   lcUserId      = fCParam("Masmovil","cancelAppointmentUserId")
   lcPassword    = fCParam("Masmovil","cancelAppointmentPass")
   lcUriPath     = fCParam("Masmovil","cancelAppointmentUriPath")
   lcUriQuery    = ""
   lcUriQueryVal = ""
   liLogRequest  = fIParam("Masmovil","cancelAppointmentLogRequest")
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
                           OUTPUT loResponseJson). 

RETURN "".

FINALLY:
   IF VALID-OBJECT(loRequestjson)
   THEN DELETE OBJECT loRequestjson.

   /* loResponseJson is not used */
   IF VALID-OBJECT(loResponseJson)
   THEN DELETE OBJECT loResponseJson.
END FINALLY.

