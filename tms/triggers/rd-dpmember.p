TRIGGER PROCEDURE FOR REPLICATION-DELETE OF DPMember.

{HPD/HPDConst.i}
{HPD/HPDTrigger.i}

&IF {&DPMEMBER_DELETE_TRIGGER_ACTIVE} &THEN

{triggers/replog_tenantname.i}

IF NEW DPMember
THEN RETURN.

IF DPMember.HostTable  = "MobSub"         AND
   DPMember.ValidFrom <= DPMember.ValidTo AND
   DPMember.ValidTo   >= ADD-INTERVAL(DATE(MONTH(TODAY),1,YEAR(TODAY)), -6, "months")
THEN DO:
   CREATE Common.RepLog.
   ASSIGN
      Common.RepLog.TableName  = "DPMember"
      Common.RepLog.EventType  = "DELETE"
      Common.RepLog.EventTime  = NOW
      Common.RepLog.TenantName = fRepLogTenantName(BUFFER DPMember:HANDLE)
      Common.RepLog.KeyValue  = {HPD/keyvalue.i DPMember . {&HPDKeyDelimiter} KeyValue DPMemberId}
      .
END.

&ENDIF