TRIGGER PROCEDURE FOR REPLICATION-WRITE OF CustContact OLD BUFFER Oldbuf.

DEF VAR llResult AS LOG NO-UNDO.

IF NEW(CustContact) THEN DO:
   CREATE Common.RepLog.
   ASSIGN
      Common.RepLog.RecordId  = RECID(CustContact)
      Common.RepLog.TableName = "CustContact"
      Common.RepLog.EventType = "CREATE"
      Common.RepLog.KeyValue  = STRING(CustContact.CustNum) + CHR(255) +
                                STRING(CustContact.CustType)
      Common.RepLog.EventTS   = DATETIME(TODAY,MTIME).
END. /* IF NEW(CustContact) THEN DO: */
ELSE DO:
   BUFFER-COMPARE CustContact USING
      CustNum
      HonTitle 
      FirstName
      CustName
      SurName2 
      Birthday
      OrgId
      CustIdType
      Nationality
      Language
      SMSNumber
      Phone
      Email
   TO Oldbuf SAVE RESULT IN llResult.

   IF NOT llResult THEN DO:
      CREATE Common.RepLog.
      ASSIGN
         Common.RepLog.RecordId  = RECID(CustContact)
         Common.RepLog.TableName = "CustContact"
         Common.RepLog.EventType = "MODIFY"
         Common.RepLog.KeyValue  = STRING(Oldbuf.CustNum) + CHR(255) +
                                   STRING(Oldbuf.CustType)
         Common.RepLog.EventTS   = DATETIME(TODAY,MTIME).
   END. /* IF NOT llResult THEN DO: */
END. /* ELSE DO: */

