{Func/triggerfunc.i}

DEF VAR liToday     AS INT NO-UNDO.
DEF VAR liYesterDay AS INT NO-UNDO.

DEF STREAM sActivity.

ASSIGN 
   liToday      = YEAR(today)     * 10000 + MONTH(today)     * 100 + DAY(Today)
   liYesterday  = YEAR(today - 1) * 10000 + MONTH(today - 1) * 100 + DAY(Today - 1).

OUTPUT STREAM sActivity to /opt/tools/rerate/status.txt.


PUT STREAM sActivity UNFORMATTED "CurrentValue;" fTriggerRateQueue() SKIP.

FIND FIRST ActivityCounter WHERE 
           ActivityCounter.Brand      = "1"            AND 
           ActivityCounter.acDate     = Today          AND 
           AcTivityCounter.ACValue    = "TRIGGERRATE"  AND 
           ActivityCounter.ACSubValue = "QTY"      NO-LOCK NO-ERROR.

IF NOT AVAIL ActivityCounter THEN  PUT STREAM sActivity UNFORMATTED litoday ";N/A" SKIP.
ELSE PUT STREAM sActivity UNFORMATTED liToday ";" ActivityCounter.IntValue SKIP.


FIND FIRST ActivityCounter WHERE 
           ActivityCounter.Brand      = "1"                AND 
           ActivityCounter.ACDate     = Today - 1          AND 
           AcTivityCounter.ACValue    = "TRIGGERRATE"      AND 
           ActivityCounter.ACSubValue = "QTY"      NO-LOCK NO-ERROR.

IF NOT AVAIL ActivityCounter THEN PUT STREAM sActivity UNFORMATTED liYesterday ";N/A" SKIP.
ELSE PUT STREAM sActivity UNFORMATTED liYesterday ";" ActivityCounter.IntValue SKIP.

                      
OUTPUT STREAM sActivity CLOSE.                      
                      
