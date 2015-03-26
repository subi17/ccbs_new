DEFINE INPUT PARAMETER pcInputFile    AS CHARACTER NO-UNDO. 
DEFINE INPUT PARAMETER pcLogFile      AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER pcPOS          AS CHARACTER NO-UNDO. 
DEFINE INPUT PARAMETER piToStatus     AS INTEGER NO-UNDO. 
DEFINE INPUT PARAMETER plSimulated    AS LOGICAL   NO-UNDO. 

DEFINE VARIABLE cCLI AS CHARACTER NO-UNDO. 

DEFINE STREAM sMSISDN.
DEFINE STREAM sLog.

OUTPUT STREAM sLog TO VALUE(pcLogFile).
INPUT STREAM sMSISDN FROM VALUE(pcInputFile).

{commpaa.i}
katun = "rafaeldv".
gcBrand  = "1".
{timestamp.i}
{msisdn.i}

def buffer msisdnbuf for msisdn.
DEFINE VARIABLE ldTS AS DECIMAL NO-UNDO. 
ldTS = fMakeTS().

REPEAT:
   IMPORT STREAM sMSISDN UNFORMATTED cCLI.

   cCLI = TRIM(cCLI).
   FIND FIRST msisdnnumber where
      msisdnnumber.cli = cClI NO-LOCK.
      
   FIND FIRST msisdn where msisdn.brand = gcBrand and 
      msisdn.cli = msisdnnumber.cli and 
      msisdn.statuscode = 0 and
      msisdn.validto > ldTS NO-LOCK NO-ERROR.
   IF NOT AVAIL msisdn THEN DO:
      put stream sLog unformatted msisdnnumber.cli "|ERROR:not correct status" skip.
      NEXT.
   END.

   /* check that no duplicate timestamps exists */
   find first msisdnbuf where
      msisdnbuf.cli = msisdn.cli and
      msisdnbuf.validto > ldTS and
      rowid(msisdnbuf) ne rowid(msisdn) NO-LOCK NO-ERROR.
   IF AVAIL msisdnbuf then do:
      put stream sLog unformatted msisdn.cli "|ERROR:duplicate timestamp found" skip.
      NEXT.
   end.

   /* check that no active subscription exists */
   find mobsub where
        mobsub.cli = msisdn.cli NO-LOCK no-error.
   IF AVAIL mobsub THEN DO:
      put stream sLog unformatted msisdn.cli "|ERROR:active subscription found" skip.
      NEXT.
   END.
      
   if msisdn.pos ne pcPOS then do:
      put stream sLog unformatted msisdn.cli "|ERROR:stock is " msisdn.pos skip.
      NEXT.
   END.

 
   /* change status */
   IF NOT plSimulated THEN DO:
          fMakeMSIDNHistory(recid(msisdn)).
          assign
             msisdn.statuscode = piToStatus.
   END.
  /* put stream sLog unformatted msisdn.cli "|OK:" skip.  */

END.

INPUT STREAM sMSISDN CLOSE.
OUTPUT STREAM sLog CLOSE.

