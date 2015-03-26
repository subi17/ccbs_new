TRIGGER PROCEDURE FOR REPLICATION-WRITE OF MsRequest OLD BUFFER Oldbuf.

DEF VAR llResult AS LOG NO-UNDO.

IF NEW(MsRequest) THEN DO:
   CREATE Mobile.RepLog.
   ASSIGN
      Mobile.RepLog.RecordId  = RECID(MsRequest)
      Mobile.RepLog.TableName = "MsRequest"
      Mobile.RepLog.EventType = "CREATE"
      Mobile.RepLog.KeyValue  = STRING(MsRequest.MsRequest) + CHR(255) + 
                                STRING(MsRequest.ReqType) + CHR(255) + 
                                STRING(MsRequest.ReqStatus)
      Mobile.RepLog.EventTS   = DATETIME(TODAY,MTIME).
END. /* IF NEW(MsRequest) THEN DO: */
ELSE DO:
   BUFFER-COMPARE MsRequest USING
      ReqStatus 
      DoneStamp 
      MsRequest
      MsSeq
      CLI
      CustNum
      ReqType
      UserCode
      ReqSource 
      CreStamp
      ActStamp
      ReqCParam1
      ReqCParam2
      ReqCParam3
      ReqCParam4
      ReqCParam5
      ReqIParam1
      ReqIParam2
      ReqIParam3
      ReqIParam4
      ReqDParam1
      TO Oldbuf SAVE RESULT IN llResult.

   IF NOT llResult THEN DO:
      CREATE Mobile.RepLog.
      ASSIGN
         Mobile.RepLog.RecordId  = RECID(MsRequest)
         Mobile.RepLog.TableName = "MsRequest"
         Mobile.RepLog.EventType = "MODIFY"
         Mobile.RepLog.KeyValue  = STRING(Oldbuf.MsRequest) + CHR(255) + 
                                   STRING(Oldbuf.ReqType) + CHR(255) + 
                                   STRING(Oldbuf.ReqStatus) + CHR(255) + 
                                   STRING(Msrequest.ReqStatus)
         Mobile.RepLog.EventTS   = DATETIME(TODAY,MTIME).
   END. /* IF NOT llResult THEN DO: */
END. /* ELSE DO: */
