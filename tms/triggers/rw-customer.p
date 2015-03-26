TRIGGER PROCEDURE FOR REPLICATION-WRITE OF Customer OLD BUFFER Oldbuf.

DEF VAR llResult AS LOG NO-UNDO.

IF NEW(Customer) THEN DO:
   CREATE Common.RepLog.
   ASSIGN
      Common.RepLog.RecordId  = RECID(Customer)
      Common.RepLog.TableName = "Customer"
      Common.RepLog.EventType = "CREATE"
      Common.RepLog.KeyValue  = STRING(Customer.CustNum)
      Common.RepLog.EventTS   = DATETIME(TODAY,MTIME).
END. /* IF NEW(Customer) THEN DO: */
ELSE DO:
   BUFFER-COMPARE Customer USING
      CustNum
      CustId 
      OrgId
      FirstName
      CustName
      SurName2 
      ZipCode
      Region
      Language
      DelType
      CompanyName
      Profession
      Address
      PostOffice
      Country
      Nationality
      Email
      Phone
      SMSNumber
      BankAcct
   TO Oldbuf SAVE RESULT IN llResult.

   IF NOT llResult THEN DO:
      CREATE Common.RepLog.
      ASSIGN
         Common.RepLog.RecordId  = RECID(Customer)
         Common.RepLog.TableName = "Customer"
         Common.RepLog.EventType = "MODIFY"
         Common.RepLog.KeyValue  = STRING(Oldbuf.CustNum)
         Common.RepLog.EventTS   = DATETIME(TODAY,MTIME).
   END. /* IF NOT llResult THEN DO: */
END. /* ELSE DO: */
