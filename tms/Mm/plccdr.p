/* ----------------------------------------------------------------------
  MODULE .......: PLCCDR.P
  TASK .........: OnLine Reader for Prepaid lifecycle CDRs 
  APPLICATION ..: TMS
  AUTHOR .......: as
  CREATED ......: 02/2009
  CHANGED ......: 
  Version ......: xfera 
  ---------------------------------------------------------------------- */

{Syst/commpaa.i}
katun   = "OnlineReader".
gcBrand = "1".
{Func/timestamp.i}
{Func/detailvalue.i}
{Syst/tmsconst.i}
{Func/fmakemsreq.i}

DEFINE STREAM sCDR.

DEFINE NEW SHARED VARIABLE callrec AS CHARACTER NO-UNDO.
DEFINE VARIABLE liLoop         AS INTEGER   NO-UNDO.
DEFINE VARIABLE lcTemp         AS CHARACTER NO-UNDO.
DEFINE VARIABLE liPort         AS INTEGER   NO-UNDO.
DEFINE VARIABLE liPause        AS INTEGER   NO-UNDO INIT 60.
DEFINE VARIABLE llOL           AS LOGICAL   NO-UNDO INIT TRUE.
DEFINE VARIABLE llLeave        AS LOGICAL   NO-UNDO FORMAT "Yes/No".
DEFINE VARIABLE lcCDR          AS CHARACTER NO-UNDO.
DEFINE VARIABLE ldaFrmDate     AS DATE      NO-UNDO.
DEFINE VARIABLE lcFrmTime      AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcFileName     AS CHARACTER NO-UNDO.
DEFINE VARIABLE liFileAmt      AS INTEGER   NO-UNDO.
   
DEFINE VARIABLE pcVersion      AS CHARACTER NO-UNDO. 
DEFINE VARIABLE pcCDRType      AS CHARACTER NO-UNDO.  
DEFINE VARIABLE pcMSISDN       AS CHARACTER NO-UNDO. 
DEFINE VARIABLE pcOrigCDRType  AS CHARACTER NO-UNDO. 
DEFINE VARIABLE ocResult       AS CHARACTER NO-UNDO.
  

FORM
   liPort     COLUMN-LABEL "PORT"   FORMAT "9999"
   ldaFrmDate COLUMN-LABEL "DATE"
   lcFrmTime  COLUMN-LABEL "TIME"
WITH ROW 2 CENTERED WIDTH 60 TITLE " LAST SAVED CALL (ONLINE STARTED " +
   STRING(today,"99-99-99") + " " + STRING(time,"HH:MM:SS") + ")"
   FRAME clog.

FORM
   callrec at 2  NO-LABEL FORMAT "x(75)" 
WITH 
   ROW 7 SCROLL 1 11 DOWN CENTERED WIDTH 78 
   TITLE "   M E S S A G E S   &   E R R O R S   " FRAME err.

ASSIGN
   liPause = 60
   llOL    = TRUE
   liPort  = 2260.

DISP 
   liPort
   " WAIT..." @ lcFrmTime
WITH FRAME clog.

DISP " " @ callrec WITH FRAME err.

If NOT llOL THEN DISP
   "Reading CDR file:" @ callrec WITH FRAME err.

ehto = 3.
RUN ufkey.
   
RUN ufxkey(8,3).

IF NOT llOL THEN INPUT STREAM sCDR FROM VALUE(lcFileName).
   
pcVersion = "YS0100".

PLCCDR:
DO WHILE TRUE:
   
   ASSIGN
      pcCDRType = ""
      pcMSISDN = ""
      pcOrigCDRType = "".

   IF llOL THEN CALL cdr liPort liPause "callrec".
   ELSE DO:

      IMPORT STREAM sCDR UNFORMATTED callrec.

      liFileAmt = liFileAmt + 1.
   
   END.

   IF callrec BEGINS "*" OR callrec BEGINS "#" THEN DO:
      
      DISP
         callrec
      WITH FRAME err.
      
      DOWN WITH FRAME err.
      
      IF FRAME-DOWN = 10 THEN SCROLL UP.
      
      PAUSE 0.
   
      NEXT PLCCDR.

   END.
   
   IF llOL AND callrec = "" then do:

      READKEY PAUSE 0.

      llLeave = FALSE.
      
      CASE LASTKEY:
        WHEN KEYCODE("F8") OR WHEN KEYCODE("8") THEN
           MESSAGE
              "ARE YOU SURE YOU WANT TO STOP READING THE CDRs (Y/N) ?"
           UPDATE llLeave.
      END.

      IF llLeave THEN LEAVE PLCCDR.
      ELSE            NEXT  PLCCDR.

   END.

   DO liLoop = 1 TO NUM-ENTRIES(callrec,"|"):

      ASSIGN
         lcTemp = ENTRY(liLoop,callrec,"|")
         lcTemp = TRIM(lcTemp).
         
      IF liLoop = 1 THEN lcCDR = lcTemp.
      ELSE               lcCDR = lcCDR + "|" + lcTemp.

   END.
  
   DO TRANSACTION:
      
      ASSIGN
         pcCDRType = ENTRY(fGetPosition(pcVersion,"Record type"), lcCDR, "|") +
                     ENTRY(fGetPosition(pcVersion,"Format version"),lcCDR,"|").
         pcMSISDN = 
            ENTRY(fGetPosition(pcVersion,"Subscriber number"),lcCDR,"|").
         pcOrigCDRType = 
            ENTRY(fGetPosition(pcVersion,"Original cdr type"), lcCDR, "|").
      
      fPLCRequest(fMakeTS(),
                  pcMSISDN,
                  pcOrigCDRType,
                  lcCDR,
                  "5",
                  OUTPUT ocResult).
   END.

END.

IF NOT llOL THEN DO:

   MESSAGE 
      "File reading is finished ! " + STRING(liFileAmt) + " CDRs"
   VIEW-AS ALERT-BOX TITLE " CDRs from file ".

END.
