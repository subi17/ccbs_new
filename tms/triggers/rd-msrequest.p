TRIGGER PROCEDURE FOR REPLICATION-DELETE OF MsRequest.

{HPD/HPDConst.i}
{Syst/tmsconst.i}

IF NEW MsRequest
THEN RETURN.

{triggers/msreqcounter.i}

fCreateMsReqCounter(MsRequest.ReqType, MsRequest.ReqStatus, -1).

&IF {&MSREQUEST_DELETE_TRIGGER_ACTIVE} &THEN

IF ( MsRequest.ReqSource > "" AND LOOKUP(MsRequest.ReqSource,{&REQUEST_SOURCES_HPD}) = 0 ) OR
   LOOKUP(STRING(MsRequest.ReqType),{&REQTYPES_HPD}) EQ 0
THEN RETURN.

CREATE Mobile.RepLog.
ASSIGN
   Mobile.RepLog.TableName = "MsRequest"
   Mobile.RepLog.EventType = "DELETE"
   Mobile.RepLog.EventTime = NOW
   Mobile.RepLog.KeyValue  = STRING(MsRequest.MsRequest)
   .

&ENDIF