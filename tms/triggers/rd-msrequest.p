TRIGGER PROCEDURE FOR REPLICATION-DELETE OF MsRequest.

IF NEW MsRequest
THEN RETURN.

{triggers/msreqcounter.i}

fCreateMsReqCounter(MsRequest.ReqType, MsRequest.ReqStatus, -1).

CREATE Mobile.RepLog.
ASSIGN
   Mobile.RepLog.RecordId  = RECID(MsRequest)
   Mobile.RepLog.TableName = "MsRequest"
   Mobile.RepLog.EventType = "DELETE"
   Mobile.RepLog.KeyValue  = STRING(Msrequest.MsRequest) + CHR(255) +
                             STRING(Msrequest.ReqType) + CHR(255) +
                             STRING(Msrequest.ReqStatus)
   Mobile.RepLog.EventTS   = DATETIME(TODAY,MTIME).
