TRIGGER PROCEDURE FOR REPLICATION-DELETE OF SubSer.

{HPD/HPDConst.i}
{Syst/tmsconst.i}

&IF {&SUBSER_DELETE_TRIGGER_ACTIVE} &THEN

IF NEW SubSer
THEN RETURN.

{triggers/subser.i}

IF LOOKUP(SubSer.ServCom,{&HPD_SERVICES}) = 0
THEN RETURN.
   
FIND SubSer_new EXCLUSIVE-LOCK  WHERE
     SubSer_new.MsSeq = Subser.MsSeq AND
     Subser_new.ServCom = Subser.ServCom AND
     Subser_new.SSdate = Subser.SSdate NO-ERROR.
IF AVAIL Subser_new THEN DELETE Subser_new.

CREATE Mobile.RepLog.
ASSIGN
   Mobile.RepLog.TableName = "SubSer"
   Mobile.RepLog.EventType = "DELETE"
   Mobile.RepLog.EventTime = NOW
   Mobile.RepLog.KeyValue  = {HPD/keyvalue.i SubSer . {&HPDKeyDelimiter} MsSeq ServCom SSDate}
   .

&ENDIF
