
DEF VAR lcGroupCodes AS CHAR NO-UNDO.
DEF VAR lcGroupCode  AS CHAR NO-UNDO.
DEF VAR liCount      AS INT  NO-UNDO.
DEF VAR liNum        AS INT  NO-UNDO.

lcGroupCodes = "CONTF10,CONTF20,CONTF20D,CONTF30,CONTF40,CONTF55,CONTS15,CONTS20,CONTS25,CONTS30,CONTS35,CONTS39,CONTDATA,CONTD2,CONTD3,CONTD4,CONTD9".

OUTPUT TO "/store/riftp/pupu_dumps/logs/update_msowner_tariff_bundle.txt".

DO liCount = 1 TO NUM-ENTRIES(lcGroupCodes):

    lcGroupCode = ENTRY(liCount,lcGroupCodes).

    FOR FIRST ServiceLimit WHERE
              ServiceLimit.GroupCode = lcGroupCode NO-LOCK,
        EACH  MServiceLimit WHERE
              MServiceLimit.SlSeq = ServiceLimit.SlSeq AND
              MServiceLimit.DialType = ServiceLimit.DialType AND
              MServiceLimit.EndTS >= 20130901 NO-LOCK:

        liNum = liNum + 1.
        STATUS DEFAULT STRING(liNum).

        DO TRANSACTION:
           FIND FIRST MsOwner WHERE
                      MsOwner.MsSeq  = MServiceLimit.MsSeq  AND
                      MsOwner.TsEnd >= MServiceLimit.FromTS AND
                      MsOwner.TsBeg <= MServiceLimit.EndTS
                EXCLUSIVE-LOCK NO-ERROR.
           IF AVAIL MsOwner THEN DO:
              MsOwner.TariffBundle = ServiceLimit.GroupCode.

              PUT UNFORMATTED
                  STRING(MsOwner.MsSeq) "|" MsOwner.CLI "|"
                  ServiceLimit.GroupCode "|" STRING(MServiceLimit.SlSeq) "|"
                  STRING(MServiceLimit.DialType) "|" STRING(MServiceLimit.FromTS) "|"
                  STRING(MServiceLimit.EndTS) "|" STRING(MsOwner.TsBeg) "|" STRING(MsOwner.TsEnd) "|"
                  MsOwner.TariffBundle SKIP.
           END.
        END.
        RELEASE MsOwner.
    END.
END.
OUTPUT CLOSE.