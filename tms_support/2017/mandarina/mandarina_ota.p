/* ----------------------------------------------------------------------
  MODULE .......: mandarina_ota.p
  TASK .........: Read input file; Create MEMO for each MSISDN in input file 
  APPLICATION ..: tms
  AUTHOR .......: jotorres & ilsavola
  CREATED ......: 09/2017
  Version ......: yoigo
---------------------------------------------------------------------- */

/* includes */
{Syst/commpaa.i}
Syst.Var:gcBrand = "1".

/* Folders and labels. */ 
DEF VAR lcInputFile AS CHAR NO-UNDO INITIAL "/tmp/OTA_KO.csv".           /* ¡¡Remember to check for each run!! */
DEF VAR lcLogFile   AS CHAR NO-UNDO INITIAL "/tmp/OTA_KO.log".           /* ¡¡Remember to check for each run!! */  
DEF VAR lcMemoTitle AS CHAR NO-UNDO INITIAL "OTA KO".                    /* ¡¡Remember to check for each run!! */ 
DEF VAR lcMemoText  AS CHAR NO-UNDO INITIAL "Migración de red - OTA KO". /* ¡¡Remember to check for each run!! */

DEF VAR lcMSISDN    AS CHAR NO-UNDO. 
DEF VAR lcLine      AS CHAR.

DEF STREAM lcInputStr.
DEF STREAM lcLogStr.

INPUT STREAM lcInputStr FROM VALUE(lcInputFile).
OUTPUT STREAM lcLogStr TO VALUE(LcLogFile).
REPEAT:
   IMPORT STREAM lcInputStr UNFORMATTED lcLine.
   lcMSISDN = TRIM(ENTRY(1, lcLine)).

   /* Removing prefix */
   IF LENGTH(lcMSISDN) EQ 11 THEN
      lcMSISDN = SUBSTRING(lcMSISDN, 3, 9).
  
   /* Check subscription */     
   FIND FIRST mobsub WHERE
              mobsub.Brand EQ Syst.Var:gcBrand AND
              mobsub.CLI   EQ lcMSISDN 
         USE-INDEX CLI NO-LOCK NO-ERROR.
   IF NOT AVAILABLE mobsub THEN DO:
      PUT STREAM lcLogStr UNFORMATTED
         lcLine + ";" + STRING(TIME,"hh:mm:ss") + ";ERROR:MSISDN_not_found" SKIP.
      NEXT.
   END.  

   Func.Common:mWriteMemoWithType("Mobsub",
                     mobsub.MsSeq,
                     mobsub.CustNum,
                     lcMemoTitle,
                     lcMemoText,
                     "Service",      /* memo type */
                     "Sistema").     /* creator */
   PUT STREAM lcLogStr UNFORMATTED
     lcLine + ";" + STRING(TIME,"hh:mm:ss") + ";OK" SKIP.
   
END.
INPUT STREAM lcInputStr CLOSE.
OUTPUT STREAM lcLogStr CLOSE.

