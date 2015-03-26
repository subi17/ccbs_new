
/* read msisdn from a list */

{commpaa.i}
katun = "rafaeldv".
gcBrand  = "1".
{timestamp.i}
{msisdn.i}
{tmsconst.i}

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
OUTPUT STREAM sout TO "yot_573_fix.log".

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


   IF bMSISDN.StatusCode NE 1 THEN NEXT.
 
   /* find the old one */
   FIND NEXT bMSISDN WHERE 
              bMSISDN.Brand = gcbrand and 
              bMSISDN.CLI = lcCLI  NO-LOCK NO-ERROR.
   ASSIGN 
   lcOldStock = bMSISDN.POS
   liOldStatusCode = bMSISDN.StatusCode.

   /* get again the new one */
   FIND FIRST bbMSISDN WHERE 
              bbMSISDN.Brand = gcbrand and 
              bbMSISDN.CLI = lcCLI  NO-LOCK NO-ERROR.

        /* change status and stock  */
   IF NOT lcSimulated THEN DO:
             fMakeMSIDNHistory(recid(bbmsisdn)).
             assign
                msisdn.statuscode = liOldStatusCode
                msisdn.pos =  lcOldStock.
    END.
      put stream sOut unformatted bmsisdn.cli "|" lcOldStock "|" STRING(liOldStatusCode)  skip.
  
END.

OUTPUT STREAM sout CLOSE.
INPUT STREAM sin CLOSE.
