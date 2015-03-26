DEFINE VARIABLE count     AS INTEGER   NO-UNDO.
DEFINE VARIABLE rmcounter AS INTEGER   NO-UNDO.
DEFINE VARIABLE tmcounter AS INTEGER   NO-UNDO.
DEFINE VARIABLE rminvseq  AS INTEGER   NO-UNDO.
DEFINE VARIABLE ttCLI     AS CHARACTER
        FORMAT "x(9)"
        LABEL "Subscription"
NO-UNDO.

DISPLAY "This script will remove the cdrs, counters and invseq's." 
        WITH FRAME a.

UPDATE ttCLI WITH FRAME b.

FIND FIRST mobsub WHERE
    mobsub.cli = ttCLI
NO-LOCK NO-ERROR.

IF NOT AVAIL mobsub then do:
   MESSAGE "Not found " ttCLI VIEW-AS ALERT-BOX ERROR.
   quit.
end.

FOR EACH mobcdr EXCLUSIVE-LOCK WHERE cli = ttCLI:

    DISPLAY cli gsmbnr datest FORMAT "99.99.99". PAUSE 0.
    DELETE mobcdr.
    count = count + 1.

END.

FOR EACH prepcdr EXCLUSIVE-LOCK WHERE cli = ttCLI:

    DISPLAY cli gsmbnr datest FORMAT "99.99.99". PAUSE 0.
    DELETE prepcdr.
    count = count + 1.

END.


FOR EACH servicelcounter EXCLUSIVE-LOCK WHERE 
         servicelcounter.msseq = mobsub.MsSeq:

    DISPLAY servicelcounter. PAUSE 0.
    DELETE servicelcounter.
    rmcounter = rmcounter + 1.

END.

FOR EACH invseq EXCLUSIVE-LOCK WHERE
         invseq.msseq = mobsub.msseq:

    DISPLAY invseq. PAUSE 0.
    DELETE invseq.
    rminvseq = rminvseq + 1.

END.

FOR EACH tmcounter EXCLUSIVE-LOCK  where
         tmcounter.msseq = mobsub.msseq:
   DISPLAY tmcounter. PAUSE 0.
   delete tmcounter.
   tmcounter = tmcounter + 1.
end.

DISPLAY count LABEL "Removed cdrs"
        rmcounter LABEL "Removed rating counters"
        tmcounter LABEL "Removed ticket management counters"
        rminvseq LABEL "Removed invseq's".
