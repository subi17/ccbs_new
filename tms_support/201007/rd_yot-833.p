
{commpaa.i}
katun = "rafaeldv".
gcBrand  = "1".
{timestamp.i}
{msisdn.i}

DEFINE VARIABLE lcPos AS CHARACTER NO-UNDO. 
DEFINE VARIABLE liToStatus AS INTEGER NO-UNDO. 
DEFINE VARIABLE llSimulated AS LOG NO-UNDO. 
DEFINE STREAM sout.
DEF BUFFER bMSISDN FOR MSISDN.


lcPos = "ONLINE".
liToStatus = 1. 
llSimulated = FALSE.
OUTPUT STREAM sout TO "yot-833_part2.log".

FOR EACH bMSISDN NO-LOCK WHERE
         bMSISDN.CLI BEGINS "6336" AND
         bMSISDN.StatusCode = 0 AND
         bMSISDN.POS = "",
    FIRST MSISDNNumber NO-LOCK WHERE
          MSISDNNumber.CLI = bMSISDN.CLI AND
          MSISDNNumber.Rank = 0 
         BREAK BY bMSISDN.CLI:
         /* Only newest with same CLI is accepted; first is newest */
        IF NOT FIRST-OF(bMSISDN.CLI) THEN NEXT.
        
        IF NOT llSimulated THEN DO:
            fMakeMSIDNHistory(recid(bmsisdn)).
            ASSIGN
               msisdn.pos = lcPos
               msisdn.statuscode = liToStatus.
            RELEASE msisdn. 
        END.
        PUT STREAM sout UNFORMATTED bMSISDN.CLI + "|OK" SKIP.
END.

OUTPUT STREAM sout CLOSE.


