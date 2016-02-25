TRIGGER PROCEDURE FOR REPLICATION-WRITE OF MsRequest OLD BUFFER oldMsRequest.

{tmsconst.i}
{HPD/HPDConst.i}

&IF {&MSREQUEST_WRITE_TRIGGER_ACTIVE} &THEN

/* If this is a new MsRequest and MsRequest requestsource is not hpd source
   or request type is not hpd req type we won't send the information */
IF NEW(MsRequest) AND
   ( ( MsRequest.ReqSource > AND LOOKUP(MsRequest.ReqSource,{&REQUEST_SOURCES_HPD}) = 0 ) OR
     LOOKUP(STRING(MsRequest.ReqType),{&REQTYPES_HPD}) EQ 0 )
THEN RETURN.

CREATE Mobile.RepLog.
ASSIGN
   Mobile.RepLog.TableName = "MsRequest"
   Mobile.RepLog.EventType = (IF NEW(MsRequest)
                               THEN "CREATE"
                               ELSE IF ( MsRequest.ReqSource > "" AND LOOKUP(MsRequest.ReqSource,{&REQUEST_SOURCES_HPD}) = 0 ) OR
                                       LOOKUP(STRING(MsRequest.ReqType),{&REQTYPES_HPD}) EQ 0
                               THEN "DELETE"                               
                               ELSE "MODIFY")
   Mobile.RepLog.EventTime = NOW
   .

IF Mobile.RepLog.EventType = "DELETE" 
THEN Mobile.RepLog.KeyValue = STRING(MsRequest.MsRequest).
ELSE Mobile.RepLog.RowID    = STRING(ROWID(MsRequest)).


IF NOT NEW(MsRequest)
THEN DO: 
   DEFINE VARIABLE llSameValues AS LOGICAL NO-UNDO.

   BUFFER-COMPARE MsRequest USING
      MsRequest
   TO oldMsRequest SAVE RESULT IN llSameValues.
   
   IF NOT llSameValues
   THEN DO:  
     
      CREATE Mobile.RepLog.
      ASSIGN
         Mobile.RepLog.TableName = "MsRequest"
         Mobile.RepLog.EventType = "DELETE"
         Mobile.RepLog.EventTime = NOW
         Mobile.RepLog.KeyValue  = STRING(oldMsRequest.MsRequest)
         .
   END.
END.

&ENDIF