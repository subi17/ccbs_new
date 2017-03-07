DEFINE VARIABLE  lcInputFile    AS CHARACTER NO-UNDO. 
DEFINE VARIABLE  lcLogFile      AS CHARACTER NO-UNDO.
DEFINE VARIABLE  lcPOS          AS CHARACTER NO-UNDO. 
DEFINE VARIABLE  liToStatus     AS INTEGER NO-UNDO. 
DEFINE VARIABLE llSimulated    AS LOGICAL   NO-UNDO. 
DEFINE VARIABLE cCLI AS CHARACTER NO-UNDO. 

DEFINE STREAM sMSISDN.
DEFINE STREAM sLog.

lcLogFile = "yot_633.log".
lcInputFile = "yot_633.input".
lcPOS = "VIP".
liToStatus = 1.
llsimulated = FALSE.


OUTPUT STREAM sLog TO VALUE(lcLogFile).
INPUT STREAM sMSISDN FROM VALUE(lcInputFile).

{Syst/commpaa.i}
katun = "rafaeldv".
gcBrand  = "1".
{Func/timestamp.i}
{Func/msisdn.i}

def buffer msisdnbuf for msisdn.
DEFINE VARIABLE ldTS AS DECIMAL NO-UNDO. 
ldTS = fMakeTS().

REPEAT:
   IMPORT STREAM sMSISDN UNFORMATTED cCLI.

   cCLI = TRIM(cCLI).
   FIND FIRST msisdnnumber where
      msisdnnumber.cli = cClI NO-LOCK NO-ERROR.

   IF NOT AVAIL msisdnnumber THEN DO:
       put stream sLog unformatted cCLI "|ERROR:MSISDNNumber not found" skip.
      NEXT.
   END.
      
   FIND FIRST msisdn where msisdn.brand = gcBrand and 
      msisdn.cli = msisdnnumber.cli NO-LOCK NO-ERROR.
   IF NOT AVAIL msisdn THEN DO:
      put stream sLog unformatted msisdnnumber.cli "|ERROR:MSISDN not found" skip.
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
       
   /* change status, mark as golden  */
   IF NOT llSimulated THEN DO:
          fMakeMSIDNHistory(recid(msisdn)).
          assign
             msisdn.pos = lcPos
             msisdn.statuscode = liToStatus.

          FIND CURRENT msisdnnumber EXCLUSIVE-LOCK NO-ERROR.
          ASSIGN msisdnnumber.rank = 1.
   END.

  
  put stream sLog unformatted msisdn.cli "|OK:" skip.

END.

INPUT STREAM sMSISDN CLOSE.
OUTPUT STREAM sLog CLOSE.

