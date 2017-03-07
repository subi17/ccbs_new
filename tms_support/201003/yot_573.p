
{Syst/commpaa.i}
katun = "rafaeldv".
gcBrand  = "1".
{Func/timestamp.i}
{Func/msisdn.i}
{Syst/tmsconst.i}

DEF BUFFER bMSISDN FOR MSISDN.
DEF BUFFER bbMSISDN FOR MSISDN. 
DEFINE VARIABLE ldTS AS DECIMAL NO-UNDO.
DEFINE VARIABLE lcSimulated AS LOGICAL NO-UNDO. 
DEFINE VARIABLE lcError AS CHARACTER NO-UNDO. 
DEFINE STREAM sLog.


FUNCTION fCheckMSISDN RETURNS CHARACTER :
   /* check that no duplicate timestamps exists */
   find first bbmsisdn where
      bbmsisdn.cli = bmsisdn.cli and
      bbmsisdn.validto > ldTS and
      rowid(bbmsisdn) ne rowid(bmsisdn) NO-LOCK NO-ERROR.
   IF AVAIL bbmsisdn then 
      RETURN "|ERROR:duplicate timestamp found".

   /* check that no active subscription exists */
   find mobsub where
        mobsub.cli = bmsisdn.cli NO-LOCK no-error.
   IF AVAIL mobsub THEN 
      RETURN "|ERROR:active subscription found".

   RETURN "".
END FUNCTION.

FUNCTION fIsGoldNumber RETURNS LOGICAL:
   DEFINE VARIABLE llGold AS LOGICAL NO-UNDO INITIAL FALSE. 
   /* avoid 633 gold numbers */
   IF bmsisdn.CLI  BEGINS "633" THEN DO:
      FIND FIRST msisdnnumber where
                 msisdnnumber.cli = bmsisdn.CLI AND 
                 msisdnnumber.rank = 1 NO-LOCK NO-ERROR.
      IF AVAIL msisdnnumber THEN  llGold = TRUE.
   END.
   RETURN llGold. 
END FUNCTION.

ldTS = fMakeTS(). 
lcSimulated = FALSE.
OUTPUT STREAM sLog TO "yot_573.log".

FOR EACH bMSISDN NO-LOCK WHERE 
         bMSISDN.POS = ({&MSISDN_STOCK_PREACTIVATED}) AND
         bMSISDN.StatusCode = 1 AND
         bMSISDN.ValidTo > ldTS  AND
         bMSISDN.Brand = gcBrand :
      
      lcError = fCheckMSISDN().
      IF lcError NE "" THEN DO:
         put stream sLog unformatted bmsisdn.cli lcError skip.
         NEXT.
      END.
 
      IF fIsGoldNumber() THEN NEXT. 

      /* change status and stock  */
      IF NOT lcSimulated THEN DO:
             fMakeMSIDNHistory(recid(bmsisdn)).
             assign
                msisdn.statuscode = 1
                msisdn.pos =  ({&MSISDN_STOCK_ONLINE}).
      END.
      put stream sLog unformatted bmsisdn.cli "|OK:" skip.    

END.

FOR EACH bMSISDN NO-LOCK WHERE
         bMSISDN.Brand = gcBrand AND 
         bMSISDN.CLI BEGINS "6335" AND
         bMSISDN.StatusCode = 0 AND
         bMSISDN.ValidTo > ldTS :
      
      lcError = fCheckMSISDN().
      IF lcError NE "" THEN DO:
         put stream sLog unformatted bmsisdn.cli lcError skip.
         NEXT.
      END.

      /* check golden 633 numbers */
      IF fIsGoldNumber() THEN NEXT. 

      /* change status and stock  */
      IF NOT lcSimulated THEN DO:
             fMakeMSIDNHistory(recid(bmsisdn)).
             assign
                msisdn.statuscode = 1
                msisdn.pos =  ({&MSISDN_STOCK_ONLINE}).
      END.
      put stream sLog unformatted bmsisdn.cli "|OK:" skip. 

END.



OUTPUT STREAM sLog CLOSE.

