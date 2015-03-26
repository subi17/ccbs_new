/* ----------------------------------------------------------------------
  MODULE .......: ROAMCDR.P
  TASK .........: OnLine Reader for Roaming Stream
  APPLICATION ..: TMS
  AUTHOR .......: kl
  CREATED ......: 14.03.07
  CHANGED ......: 27.04.07 kl RoamGPRS + special rates for LTUOM,SWEHU,DNKHU
                  20.7.2007 anttis New roaming pricing calculatiob is used.
                  10.10.2007 kl RELEASE... 

  Version ......: xfera 
  ---------------------------------------------------------------------- */

{timestamp.i}
{commali.i}
{roamtariff.i}
{cdrstream_counter.i}
{heartbeat.i}
{onlinereader_oldcdr.i}

DEF INPUT PARAMETER iiPortNum AS INT  NO-UNDO.

DEFINE STREAM sCDR.

DEFINE NEW SHARED VARIABLE callrec AS CHARACTER NO-UNDO.
DEFINE VARIABLE liLoop         AS INTEGER   NO-UNDO.
DEFINE VARIABLE lcTemp         AS CHARACTER NO-UNDO.
DEFINE VARIABLE liPause        AS INTEGER   NO-UNDO INIT 60.
DEFINE VARIABLE llOL           AS LOGICAL   NO-UNDO INIT TRUE.
DEFINE VARIABLE llLeave        AS LOGICAL   NO-UNDO FORMAT "Yes/No".
DEFINE VARIABLE lcCDR          AS CHARACTER NO-UNDO.
DEFINE VARIABLE ldaFrmDate     AS DATE      NO-UNDO.
DEFINE VARIABLE lcFrmTime      AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcFileName     AS CHARACTER NO-UNDO.
DEFINE VARIABLE liFileAmt      AS INTEGER   NO-UNDO.
DEFINE VARIABLE lcEventType    AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcVersion      AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcMediatorTime AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcMSCID        AS CHARACTER NO-UNDO.
DEFINE VARIABLE ldtTMSTime     AS DATETIME  NO-UNDO.
DEFINE VARIABLE ldaStartDate   AS DATE NO-UNDO.
DEFINE VARIABLE lcReturn       AS CHAR NO-UNDO.

DEF VAR llSaved  AS LOG    NO-UNDO.
DEF VAR lhttCDR  AS HANDLE NO-UNDO.
DEF VAR lhttGPRS AS HANDLE NO-UNDO.


FORM
   iiPortNum  COLUMN-LABEL "PORT"   FORMAT "9999"
   ldaFrmDate COLUMN-LABEL "DATE"   FORMAT "99/99/9999"
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
   ldaStartDate = TODAY
   lhttCDR  = BUFFER ttRoamCDR:HANDLE
   lhttGPRS = BUFFER ttRoamGPRS:HANDLE.

IF iiPortNum NE 2230 THEN RETURN "ERROR".

PAUSE 0.
DISP 
   iiPortNum
   " WAIT..." @ lcFrmTime
WITH FRAME clog.

PAUSE 0.
DISP " " @ callrec WITH FRAME err.

If NOT llOL THEN DISP
   "Reading CDR file:" @ callrec WITH FRAME err.

ehto = 3.
RUN ufkey.
   
/* QUIT menutext */
RUN ufxkey(8,3).

IF NOT llOL THEN INPUT STREAM sCDR FROM VALUE(lcFileName).

ROAMCDR:
DO WHILE TRUE:

   RELEASE roamcdr.RoamGPRS.
   RELEASE roamcdr.RoamCDR.
   
   IF ldaStartDate NE TODAY AND llOL THEN DO:
      lcReturn = "RESET".
      LEAVE ROAMCDR.
   END.
   
   IF llOL THEN CALL cdr iiPortNum liPause "callrec".
   ELSE DO:

      IMPORT STREAM sCDR UNFORMATTED callrec.

      liFileAmt = liFileAmt + 1.
   
   END.

   fKeepAlive("roamcdr:Roaming IN").

   IF callrec BEGINS "*" OR callrec BEGINS "#" THEN DO:
      
      DISP
         callrec
      WITH FRAME err.
      
      DOWN WITH FRAME err.
      
      IF FRAME-DOWN = 10 THEN SCROLL UP.
      
      PAUSE 0.
   
      NEXT ROAMCDR.

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

      IF llLeave THEN LEAVE ROAMCDR.
      ELSE            NEXT  ROAMCDR.

   END.

   DO liLoop = 1 TO NUM-ENTRIES(callrec,"|"):

      ASSIGN
         lcTemp = ENTRY(liLoop,callrec,"|")
         lcTemp = TRIM(lcTemp).
         
      IF liLoop = 1 THEN lcCDR = lcTemp.
      ELSE               lcCDR = lcCDR + "|" + lcTemp.

   END.
   
   EMPTY TEMP-TABLE ttRoamGPRS.
   EMPTY TEMP-TABLE ttRoamCDR.
   
   DO TRANSACTION:
      
      ASSIGN
         lcMSCID   = ENTRY(3,lcCDR,"|")
         lcVersion = ENTRY(5,lcCDR,"|").
    
      IF lcVersion = "0102" THEN ASSIGN
         lcMediatorTime = ""
         lcEventType    = ENTRY(8,lcCDR,"|").
      ELSE ASSIGN
         lcMediatorTime = ENTRY(10,lcCDR,"|")
         lcEventType    = ENTRY(11,lcCDR,"|").
      
      CASE lcEventType:
         WHEN "GPRS" THEN DO:
            CREATE ttRoamGprs.
            ttRoamGprs.CSV = lcCDR.
         END.
         WHEN "CALL" OR WHEN "SMS" THEN DO:
            CREATE ttRoamCdr.
            ttRoamCdr.CSV = lcCDR.
         END.
      END.   

      IF AVAIL ttRoamGprs OR AVAIL ttRoamCdr THEN DO:     

         ldtTMSTime = NOW.
         
         fRoamTariff(TRUE, BUFFER ttRoamCdr, BUFFER ttRoamGprs). 

         /* if this is an old cdr that came in late then check if it should 
            be saved to old db, otherwise or if this fails then save to 
            current db */
         llSaved = FALSE.
            
         CASE lcEventType:
         WHEN "GPRS" THEN DO:
            IF ttRoamGPRS.DateStart < TODAY THEN 
               llSaved = fSaveCDR2OldDB("RoamGPRS",
                                        lhttGPRS).

            IF NOT llSaved THEN DO:
               CREATE roamcdr.RoamGPRS.
               BUFFER-COPY ttRoamGPRS TO RoamGPRS.
            END.
         END.
         OTHERWISE DO:
            IF ttRoamCDR.DateStart < TODAY THEN 
               llSaved = fSaveCDR2OldDB("RoamCDR",
                                        lhttCDR).

            IF NOT llSaved THEN DO:
               CREATE roamcdr.RoamCDR.
               BUFFER-COPY ttRoamCDR TO RoamCDR.
            END.
         END.
         END CASE.
          
         /* handling time for kpi */
         fUpdateKPICounter(lcMSCID,
                           55,         
                           ldtTMSTime,
                           lcMediatorTime).
             
         IF AVAIL ttRoamCdr THEN
            DISP
               ttRoamCdr.DateStart                    @ ldaFrmDate
               STRING(ttRoamCdr.TimeStart,"HH:MM:SS") @ lcFrmTime
            WITH FRAME clog.
         
         IF AVAIL ttRoamGprs THEN
            DISP
               ttRoamGprs.DateStart                    @ ldaFrmDate
               STRING(ttRoamGprs.TimeStart,"HH:MM:SS") @ lcFrmTime
            WITH FRAME clog.
      END.

   END.

END.

HIDE FRAME err NO-PAUSE.
HIDE FRAME clog NO-PAUSE.

IF NOT llOL THEN DO:

   MESSAGE 
      "File reading is finished ! " + STRING(liFileAmt) + " CDRs"
   VIEW-AS ALERT-BOX TITLE " CDRs from file ".

END.

RETURN lcReturn.

