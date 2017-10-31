{Syst/commpaa.i}
katun = "rafaeldv".
Syst.CUICommon:gcBrand  = "1".
{Func/msisdn.i}

DEFINE VARIABLE ldTS AS DECIMAL NO-UNDO. 
DEFINE VARIABLE plSimulated AS LOGICAL NO-UNDO. 
def buffer msisdnbuf for msisdn.
DEFINE STREAM sLog.
OUTPUT STREAM sLog TO 'yot_383.log'.

plSimulated = FALSE.
ldTS = Func.Common:mMakeTS(). 

FOR EACH MSISDNbuf NO-LOCK WHERE 
         MSISDNbuf.Brand = Syst.CUICommon:gcBrand AND
         MSISDNbuf.CLI BEGINS '633996' AND
         MSISDNbuf.ValidTo > ldTS:
        /* change status */
        IF NOT plSimulated THEN DO:
              fMakeMSIDNHistory(recid(msisdnbuf)).
              assign
               msisdn.statuscode = 1.
             if msisdn.pos ne 'VIP' then assign msisdn.pos = 'VIP'.
        END.
        put stream sLog unformatted MSISDNbuf.cli "|OK" skip.
END.
OUTPUT STREAM sLog CLOSE.

