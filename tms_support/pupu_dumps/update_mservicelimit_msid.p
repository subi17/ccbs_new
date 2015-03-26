
DISABLE TRIGGERS FOR LOAD OF MServiceLimit.

DEF VAR liEvents AS INT NO-UNDO.
DEF VAR liMSID   AS INT NO-UNDO.

DEF BUFFER bMServiceLimit FOR MServiceLimit.

OUTPUT TO "/store/riftp/pupu_dumps/logs/update_mservicelimit_msid.txt".

FOR EACH MobSub WHERE
         MobSub.Brand = "1" NO-LOCK,
    EACH MServiceLimit WHERE
         MServiceLimit.MsSeq = MobSub.MsSeq   AND
         MServiceLimit.EndTS = 99999999.99999 AND
         MServiceLimit.MSID  = 0 NO-LOCK:

   liEvents = liEvents + 1.

   STATUS DEFAULT STRING(liEvents).

   DO TRANSACTION:
      FIND FIRST bMServiceLimit WHERE
                 ROWID(bMServiceLimit) = ROWID(MServiceLimit)
           EXCLUSIVE-LOCK NO-ERROR.
      IF AVAIL bMServiceLimit THEN DO:
         liMSID = NEXT-VALUE(mServiceLimit).

         FIND FIRST ServiceLCounter WHERE
                    ServiceLCounter.MsSeq  = bMServiceLimit.MsSeq AND
                    ServiceLCounter.SlSeq  = bMServiceLimit.SlSeq AND
                    ServiceLCounter.Period = 201306               AND
                    ServiceLCounter.MSID   = bMServiceLimit.MSID EXCLUSIVE-LOCK NO-ERROR.
         IF AVAIL ServiceLCounter THEN DO:
            ServiceLCounter.MSID = liMSID.
            bMServiceLimit.MSID = liMSID.
            PUT UNFORMATTED
             STRING(MobSub.MsSeq) "|" MobSub.CLI "|"
             STRING(bMServiceLimit.SlSeq) "|" STRING(bMServiceLimit.FromTS) "|"
             STRING(bMServiceLimit.EndTS) "|" STRING(bMServiceLimit.MSID) "|"
             STRING(ServiceLCounter.MSID) SKIP.
         END.
         ELSE DO:
            bMServiceLimit.MSID = liMSID.
            PUT UNFORMATTED
             STRING(MobSub.MsSeq) "|" MobSub.CLI "|"
             STRING(bMServiceLimit.SlSeq) "|" STRING(bMServiceLimit.FromTS) "|"
             STRING(bMServiceLimit.EndTS) "|" STRING(bMServiceLimit.MSID) "|" SKIP.
         END.
      END.
   END.
   RELEASE MServiceLimit.
   RELEASE ServiceLCounter.
END.
OUTPUT CLOSE.
