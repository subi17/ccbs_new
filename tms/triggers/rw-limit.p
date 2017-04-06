TRIGGER PROCEDURE FOR REPLICATION-WRITE OF Limit OLD BUFFER oldLimit.

{HPD/HPDConst.i}
{Syst/tmsconst.i}

&IF {&LIMIT_WRITE_TRIGGER_ACTIVE} &THEN

IF NEW(Limit) AND Limit.LimitType NE {&LIMIT_TYPE_Q25_DISCOUNT}
THEN RETURN.

IF NOT NEW(Limit) AND Limit.LimitType NE {&LIMIT_TYPE_Q25_DISCOUNT} AND oldLimit.LimitType NE {&LIMIT_TYPE_Q25_DISCOUNT}
THEN RETURN.

{triggers/replog_tenantname.i}

CREATE Common.RepLog.
ASSIGN
   Common.RepLog.TableName = "Limit"
   Common.RepLog.EventType = (IF NEW(Limit)
                               THEN "CREATE"
                               ELSE IF Limit.LimitType NE {&LIMIT_TYPE_Q25_DISCOUNT} AND oldLimit.LimitType = {&LIMIT_TYPE_Q25_DISCOUNT}
                               THEN "DELETE"
                               ELSE "MODIFY")
   Common.RepLog.EventTime = NOW
   Common.RepLog.TenantName = fRepLogTenantName(BUFFER Limit:HANDLE)
   .

IF Common.RepLog.EventType = "DELETE" 
THEN Common.RepLog.KeyValue = {HPD/keyvalue.i Limit . {&HPDKeyDelimiter} CustNum MsSeq LimitType FromDate}.
ELSE Common.RepLog.RowID    = STRING(ROWID(Limit)).

IF NOT NEW(Limit)
THEN DO:   
   DEFINE VARIABLE llSameValues AS LOGICAL NO-UNDO.
      
   BUFFER-COMPARE Limit USING
      CustNum MsSeq LimitType ToDate
   TO oldLimit SAVE RESULT IN llSameValues.
   
   IF NOT llSameValues
   THEN DO:
      CREATE Common.RepLog.
      ASSIGN
         Common.RepLog.TableName  = "Limit"
         Common.RepLog.EventType  = "DELETE"
         Common.RepLog.EventTime  = NOW
         Common.RepLog.TenantName = fRepLogTenantName(BUFFER oldLimit:HANDLE)
         Common.RepLog.KeyValue  = {HPD/keyvalue.i oldLimit . {&HPDKeyDelimiter} CustNum MsSeq LimitType FromDate}
         .
   END.
END.

DEFINE VARIABLE ldaDate AS DATE NO-UNDO.
DEFINE VARIABLE lii AS INTEGER INITIAL 0 NO-UNDO.
DEFINE BUFFER lbLimit FOR Limit.
FOR EACH Limit WHERE Limit.LimitType = 6 NO-LOCK
 BREAK
BY Limit.CustNum
BY Limit.MsSeq
BY Limit.LimitType
BY Limit.FromDate:

IF FIRST-OF(Limit.FromDate)
THEN ASSIGN
ldaDate = Limit.FromDate
lii = 0
.

lii = lii + 1.

IF LAST-OF(Limit.FromDate) AND ldaDate = Limit.FromDate AND lii > 1
THEN DO:
 FOR EACH lbLimit NO-LOCK WHERE
 lbLimit.CustNum = Limit.CustNum AND
 lbLimit.MsSeq = Limit.MsSeq AND
 lbLimit.LimitType = 6 AND
 lbLimit.FromDate = Limit.FromDate AND
 lbLimit.ToDate >= TODAY:

 DISPLAY lbLimit.

 END.
END.

END.

&ENDIF