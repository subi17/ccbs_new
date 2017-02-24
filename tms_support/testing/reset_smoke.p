{Syst/commpaa.i}

katun = "Cron".
gcBrand = "1".

DEFINE VARIABLE tticc   LIKE mobsub.icc.
DEFINE VARIABLE ttcli   LIKE mobsub.cli.
DEFINE VARIABLE ttmsseq LIKE mobsub.msseq.

DEFINE TEMP-TABLE ttSmokeIcc
    FIELD icc LIKE mobsub.icc.
DEFINE TEMP-TABLE ttSmokeCli
    FIELD cli LIKE mobsub.cli.
DEFINE TEMP-TABLE ttSmokeMsseq
    FIELD msseq LIKE mobsub.msseq.
DEFINE TEMP-TABLE ttSmokeOrder
    FIELD orderid LIKE order.orderid.

INPUT FROM smoke/order_icc.

REPEAT:
    CREATE ttSmokeIcc.
    IMPORT UNFORMATTED tticc.
    ASSIGN ttSmokeIcc.icc = tticc.
END.
DISP "ttSmokeIcc done.".

INPUT FROM smoke/order_cli.

REPEAT:
    CREATE ttSmokeCli.
    IMPORT UNFORMATTED ttcli.
    ASSIGN ttSmokeCli.cli = ttcli.
END.
DISP "ttSmokeCli done.".

/* Searches all mobsubs with icc in order_icc file and in same
   creates msseq temp-table for later deletions. */
FOR EACH mobsub NO-LOCK:
    FOR EACH order NO-LOCK WHERE
        order.brand = "1" AND
        order.cli = ttSmokeCli.cli:

        FIND FIRST ttSmokeCli WHERE
            ttSmokeCli.cli = order.cli
        NO-LOCK NO-ERROR.
        
        CREATE ttSmokeOrder.
        ASSIGN ttSmokeOrder.orderid = order.orderid.
    END.
    
    FIND FIRST ttSmokeIcc WHERE
        ttSmokeIcc.icc = mobsub.icc
    NO-LOCK NO-ERROR.
    
    IF NOT AVAIL ttSmokeIcc THEN NEXT.
    IF (mobsub.icc <> ttSmokeIcc.icc) THEN NEXT.

    CREATE ttSmokeMsseq.
    ASSIGN ttSmokeMsseq.msseq = mobsub.msseq.
END.
DISP "ttSmokeMsseq done.".

/* After this line we remove data with icc and msisdn
   numbers that we enter to smoke -folder. */
FOR EACH ttSmokeMsseq NO-LOCK:
    FOR EACH MNPSub NO-LOCK WHERE
        MNPSub.msseq = ttSmokeMsseq.msseq:
        
        FOR EACH MNPDetails NO-LOCK WHERE
            MNPDetails.MNPSeq = MNPSub.MNPSeq:
            DISP mnpdetails.mnpseq. PAUSE 0.
        END.
        FOR EACH MNPOperation NO-LOCK WHERE
            MNPOperation.MNPSeq = MNPSub.MNPSeq:
            DISP MNPOperation.mnpseq. PAUSE 0.
        END.
        DISP mnpsub. PAUSE 0.
    END.
    FOR EACH order NO-LOCK WHERE
        order.msseq = ttSmokeMsseq.msseq:

        FIND FIRST ttSmokeCli WHERE
            ttSmokeCli.cli = order.cli
        NO-LOCK NO-ERROR.
        
        CREATE ttSmokeOrder.
        ASSIGN ttSmokeOrder.orderid = order.orderid.
    END.
    RUN testing/reset_table.p "mobsub" "msseq" ttsmokeMsseq.msseq. PAUSE 0.
    RUN testing/reset_table.p "subser" "msseq" ttsmokeMsseq.msseq. PAUSE 0.
    RUN testing/reset_table.p "subserpara" "msseq" ttsmokeMsseq.msseq. PAUSE 0.
    RUN testing/reset_table.p "mservicelimit" "msseq" ttsmokeMsseq.msseq. PAUSE 0.
    RUN testing/reset_table.p "msowner" "msseq" ttsmokeMsseq.msseq. PAUSE 0.
    RUN testing/reset_table.p "msrequest" "msseq" ttsmokeMsseq.msseq. PAUSE 0.
    RUN testing/reset_table.p "dccli" "msseq" ttsmokeMsseq.msseq. PAUSE 0.
    RUN testing/reset_table.p "msisdn" "msseq" ttsmokeMsseq.msseq. PAUSE 0.
    RUN testing/reset_table.p "substerminal" "msseq" ttsmokeMsseq.msseq. PAUSE 0.
    RUN testing/reset_table.p "limit" "msseq" ttsmokeMsseq.msseq. PAUSE 0.
    RUN testing/reset_table.p "prepaidrequest" "msseq" ttsmokeMsseq.msseq. PAUSE 0.
    RUN testing/reset_table.p "ServiceLCounter" "msseq" ttsmokeMsseq.msseq. PAUSE 0.
    RUN testing/reset_table.p "SaldoCounter" "msseq" ttsmokeMsseq.msseq. PAUSE 0.
    RUN testing/reset_table.p "DCCounter" "msseq" ttsmokeMsseq.msseq. PAUSE 0.
    RUN testing/reset_table.p "MSBalance" "msseq" ttsmokeMsseq.msseq. PAUSE 0.
    RUN testing/reset_table.p "TMCounter" "msseq" ttsmokeMsseq.msseq. PAUSE 0.
    RUN testing/reset_table.p "MNPSub" "msseq" ttsmokeMsseq.msseq. PAUSE 0.
END.

FOR EACH ttSmokeOrder NO-LOCK:
    RUN testing/reset_table.p "orderaccessory" "orderid" ttSmokeOrder.orderid. PAUSE 0.
    RUN testing/reset_table.p "orderpayment" "orderid" ttSmokeOrder.orderid. PAUSE 0.
    RUN testing/reset_table.p "mnpprocess" "orderid" ttSmokeOrder.orderid. PAUSE 0.
END.

FOR EACH ttSmokeCli NO-LOCK:
    FOR EACH order EXCLUSIVE-LOCK WHERE
        order.cli = ttSmokeCli.cli:

        DISPLAY order.cli. PAUSE 0.
        DELETE order.
    END.
END.

FOR EACH ttSmokeIcc NO-LOCK:
    FOR EACH sim EXCLUSIVE-LOCK WHERE
        sim.icc = ttSmokeIcc.icc:

        DISP sim.icc. PAUSE 0.
        
        ASSIGN  SimStat  = 1
        MsSeq = 0.
    END.
END.

INPUT CLOSE.

