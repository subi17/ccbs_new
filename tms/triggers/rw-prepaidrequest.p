TRIGGER PROCEDURE FOR REPLICATION-WRITE OF PrepaidRequest OLD BUFFER Oldbuf.

DEF VAR llResult AS LOG NO-UNDO.

IF NEW(PrepaidRequest) THEN DO:
   CREATE OrderCanal.RepLog.
   ASSIGN
      OrderCanal.RepLog.RecordId  = RECID(PrepaidRequest)
      OrderCanal.RepLog.TableName = "PrepaidRequest"
      OrderCanal.RepLog.EventType = "CREATE"
      OrderCanal.RepLog.KeyValue  = STRING(PrepaidRequest.PPRequest)
      OrderCanal.RepLog.EventTS   = DATETIME(TODAY,MTIME).
END. /* IF NEW(PrepaidRequest) THEN DO: */
ELSE DO:
   BUFFER-COMPARE PrepaidRequest USING
      PPStatus
      PPRequest
      MsSeq
      CLI
      Request
      Source
      TSRequest
      RespCode
      TopUpAmt
      VatAmt
      Reference
   TO Oldbuf SAVE RESULT IN llResult.

   IF NOT llResult THEN DO:
      CREATE OrderCanal.RepLog.
      ASSIGN
         OrderCanal.RepLog.RecordId  = RECID(PrepaidRequest)
         OrderCanal.RepLog.TableName = "PrepaidRequest"
         OrderCanal.RepLog.EventType = "MODIFY"
         OrderCanal.RepLog.KeyValue  = STRING(Oldbuf.PPRequest)
         OrderCanal.RepLog.EventTS   = DATETIME(TODAY,MTIME).
   END. /* IF NOT llResult THEN DO: */
END. /* ELSE DO: */
