
/* read msisdn from a list */

{Syst/commpaa.i}
katun = "rafaeldv".
gcBrand  = "1".
{Func/timestamp.i}
{Func/msisdn.i}
{Syst/tmsconst.i}

DEF BUFFER bMSISDN FOR MSISDN.
DEF BUFFER bbMSISDN FOR MSISDN. 


DEFINE VARIABLE cLine AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcCLI AS CHARACTER NO-UNDO. 
DEFINE VARIABLE liCount AS INTEGER NO-UNDO. 
DEFINE VARIABLE lcOldStock AS CHARACTER NO-UNDO. 
DEFINE VARIABLE liOldStatusCode AS INTEGER NO-UNDO. 
DEFINE VARIABLE lcSimulated AS LOGICAL NO-UNDO.
DEFINE STREAM sin.
DEFINE STREAM sout.

INPUT STREAM sin FROM  "yot_573.log".
OUTPUT STREAM sout TO "yot_573_fix_used.log".

lcSimulated = FALSE.

REPEAT:
   IMPORT STREAM sin UNFORMATTED cLine.
   lcCLI  = ENTRY(1,cLine,"|").

   FIND FIRST msisdnnumber where
              msisdnnumber.cli = lcCLI AND 
              msisdnnumber.rank = 1 NO-LOCK NO-ERROR.
   IF NOT AVAIL MSISDNNumber THEN NEXT. 

   find first bmsisdn where
           bmsisdn.brand = "1" and 
           bmsisdn.cli = lcCLI  NO-LOCK no-error.
 
    IF NOT AVAIL bMSISDN THEN DO: 
    put stream sOut unformatted lcCLI " NOT FOUND" skip.
    NEXT.
    END.


   IF bMSISDN.StatusCode NE 1 THEN DO:
    put stream sOut unformatted lcCLI skip.
    NEXT.
   END.
 
END.

OUTPUT STREAM sout CLOSE.
INPUT STREAM sin CLOSE.
