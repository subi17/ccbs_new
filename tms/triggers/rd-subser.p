TRIGGER PROCEDURE FOR REPLICATION-DELETE OF SubSer.

{HPD/HPDConst.i}
{Syst/tmsconst.i}

&IF {&SUBSER_DELETE_TRIGGER_ACTIVE} &THEN

IF NEW SubSer
THEN RETURN.

DEFINE BUFFER lbSubSer FOR SubSer.

/* We will send only the newest one */
FOR
   FIRST lbSubSer FIELDS (MsSeq ServCom SSDate) NO-LOCK USE-INDEX ServCom WHERE
      lbSubSer.MsSeq   = SubSer.MsSeq  AND
      lbSubSer.ServCom = SubSer.ServCom:

   IF lbSubSer.SSDate > SubSer.SSDate
   THEN RETURN.
END.

IF LOOKUP(SubSer.ServCom,{&HPD_SERVICES}) = 0
THEN RETURN.

CREATE Mobile.RepLog.
ASSIGN
   Mobile.RepLog.TableName = "SubSer"
   Mobile.RepLog.EventType = "DELETE"
   Mobile.RepLog.EventTime = NOW
   Mobile.RepLog.KeyValue  = {HPD/keyvalue.i SubSer . {&HPDKeyDelimiter} MsSeq ServCom SSDate}
   .

&ENDIF