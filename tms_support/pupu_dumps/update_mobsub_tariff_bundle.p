
DISABLE TRIGGERS FOR LOAD OF MobSub.

DEF VAR lcGroupCodes AS CHAR NO-UNDO.
DEF VAR lcGroupCode  AS CHAR NO-UNDO.
DEF VAR liCount      AS INT  NO-UNDO.
DEF VAR liNum        AS INT  NO-UNDO.

lcGroupCodes = "CONTF10,CONTF20,CONTF20D,CONTF30,CONTF40,CONTF55,CONTS30,CONTS39,CONTDATA,CONTD2,CONTD3,CONTD4,CONTD9".

OUTPUT TO "/store/riftp/pupu_dumps/logs/update_mobsub_tariff_bundle.txt".

DO liCount = 1 TO NUM-ENTRIES(lcGroupCodes):

    lcGroupCode = ENTRY(liCount,lcGroupCodes).

    FOR FIRST ServiceLimit WHERE
              ServiceLimit.GroupCode = lcGroupCode NO-LOCK,
        EACH  MServiceLimit WHERE
              MServiceLimit.SlSeq = ServiceLimit.SlSeq AND
              MServiceLimit.DialType = ServiceLimit.DialType AND
              MServiceLimit.EndTS = 99999999.99999 NO-LOCK:

        liNum = liNum + 1.
        STATUS DEFAULT STRING(liNum).

        DO TRANSACTION:
           FIND FIRST MobSub WHERE
                      MobSub.MsSeq = MServiceLimit.MsSeq
                EXCLUSIVE-LOCK NO-ERROR.
           IF AVAIL MobSub THEN DO:
              MobSub.TariffBundle = ServiceLimit.GroupCode.

              PUT UNFORMATTED
                  STRING(MobSub.MsSeq) "|" MobSub.CLI "|"
                  ServiceLimit.GroupCode "|" STRING(MServiceLimit.SlSeq) "|"
                  STRING(MServiceLimit.DialType) "|" STRING(MServiceLimit.FromTS) "|"
                  STRING(MServiceLimit.EndTS) "|" MobSub.TariffBundle SKIP.
           END.
        END.
        RELEASE MobSub.
    END.
END.
OUTPUT CLOSE.