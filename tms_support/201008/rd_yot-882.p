
 
DEFINE VARIABLE lcToPOS          AS CHARACTER NO-UNDO. 
DEFINE VARIABLE llSimulated    AS LOGICAL   NO-UNDO. 
DEFINE VARIABLE ldTS AS DECIMAL NO-UNDO. 
DEFINE STREAM sLog.

OUTPUT STREAM sLog TO "yot-882.log".

{commpaa.i}
katun = "rafaeldv".
gcBrand  = "1".
{timestamp.i}
{msisdn.i}

def buffer msisdnbuf for msisdn.
DEF BUFFER bmsisdn FOR msisdn.
ldTS = fMakeTS().
llSimulated = FALSE. 

FOR EACH bMSISDN NO-LOCK WHERE
         bMSISDN.Brand = gcBrand AND
         ( bMSISDN.CLI BEGINS "62262262"  OR
           bMSISDN.CLI BEGINS "62298"     OR
           bMSISDN.CLI BEGINS "62299"     OR
           bMSISDN.CLI BEGINS "633633336" OR
           bMSISDN.CLI BEGINS "63363355"  OR
           bMSISDN.CLI BEGINS "63363363"  OR
           bMSISDN.CLI BEGINS "63363364"  OR
           bMSISDN.CLI BEGINS "63363365"  OR
           bMSISDN.CLI BEGINS "6338006"   OR
           bMSISDN.CLI BEGINS "6338008"   OR
           bMSISDN.CLI BEGINS "63398"     OR 
           bMSISDN.CLI BEGINS "63399") AND
         bMSISDN.ValidTo > ldTS :

   /* check that no duplicate timestamps exists */
   find first msisdnbuf where
      msisdnbuf.cli = bmsisdn.cli and
      msisdnbuf.validto > ldTS and
      rowid(msisdnbuf) ne rowid(bmsisdn) NO-LOCK NO-ERROR.
   IF AVAIL msisdnbuf then do:
      put stream sLog unformatted bmsisdn.cli "|ERROR:duplicate timestamp found" skip.
      NEXT.
   end.

   /* check that no active subscription exists */
   find mobsub where
        mobsub.cli = bmsisdn.cli NO-LOCK no-error.
   IF AVAIL mobsub THEN NEXT.


   lcToPOS = "CORE".
   IF bmsisdn.cli begins "63399" then lcToPOS = "VIP".

   /* change status and stock  */
   IF NOT llSimulated THEN DO:
          fMakeMSIDNHistory(recid(bmsisdn)).
          assign
             msisdn.pos = lctoPOS.
   END.
   put stream sLog unformatted bmsisdn.cli "|OK" skip. 

END.

OUTPUT STREAM sLog CLOSE.

