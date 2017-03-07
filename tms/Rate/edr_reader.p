/* ----------------------------------------------------------------------
  MODULE .......: edr_reader.P
  TASK .........: OnLine Reader for TARJ6 Prepaid EDRs. YPR-338 YPR-344 
  APPLICATION ..: TMS
  AUTHOR .......: anttis
  CREATED ......: 6.2.2013
  Version ......: xfera 
  ---------------------------------------------------------------------- */
{Syst/commali.i}
{Func/timestamp.i}
{Syst/tmsconst.i}
{Rate/onlinereader_oldcdr.i}
{Func/detailseq.i}
{Func/heartbeat.i}
{Func/cparam2.i}
{Rate/edr_reader.i}
{Func/log.i}
{Rate/cdr_online.i}

def INPUT PARAMETER    icFileName  as char FORMAT "x(30)" no-undo.
DEF INPUT PARAMETER    iiPort      AS INT  NO-UNDO.

SESSION:SYSTEM-ALERT-BOXES = TRUE. 
SESSION:NUMERIC-FORMAT     = "European".

DEF VAR liPause        AS INTEGER   NO-UNDO INIT 60.
DEF VAR llOL           AS LOGICAL   NO-UNDO INIT TRUE.
DEF VAR liStream       AS INTEGER   NO-UNDO. 
DEF VAR lcErrorLogFile AS CHAR      NO-UNDO.
DEF STREAM sErrorFile.
 
fLogBasic("Started").

ASSIGN
   liPause = 60
   liStream = 50
   lcErrorLogFile = fCParam("EDR","EDRErrorLog").

ASSIGN
   llOL = (icFileName = "EDR_OnLine").

DEF NEW SHARED VAR callrec AS CHAR NO-UNDO.
DEF VAR liLoop       AS INT   NO-UNDO.
DEF VAR liCDRLoop    AS INT   NO-UNDO. 
DEF VAR lcTemp       AS CHAR  NO-UNDO.
DEF VAR llLeave      AS LOG   NO-UNDO FORMAT "Yes/No".
DEF VAR lcCDR        AS CHAR  NO-UNDO.
DEF VAR ldaFrmDate   AS DATE  NO-UNDO.
DEF VAR lcFrmTime    AS CHAR  NO-UNDO.
DEF VAR liFileAmt    AS INT   NO-UNDO.
DEF VAR lcmscid      AS CHAR  NO-UNDO. 
DEF VAR lcStartDate  AS CHAR  NO-UNDO. 
DEF VAR lcStartTime  AS CHAR  NO-UNDO. 
DEF VAR cdrfile      AS CHAR  NO-UNDO.
DEF VAR liTimeout    AS INT   NO-UNDO. 
   
DEF VAR lcVersion    AS CHAR NO-UNDO. 
DEF VAR lcCDRType    AS CHAR NO-UNDO.  
DEF VAR lhttCall AS HANDLE NO-UNDO. 
lhttCall = BUFFER ttEDR:HANDLE.

DEFINE STREAM sCDR.

FORM
   cdrfile    COLON 15 label "Source"    FORMAT "x(25)" SKIP
   ldaFrmDate COLON 15 LABEL "Date" SKIP
   lcFrmTime  COLON 15 LABEL "Time" SKIP
   liCDRLoop  COLON 15 LABEL "Events read" FORMAT "zzzzzzzzz9" SKIP
   liTimeout  COLON 15 LABEL "Time outs" FORMAT   "zzzzzzzzz9" SKIP
WITH ROW 2 CENTERED WIDTH 60 TITLE " LAST SAVED CALL (ONLINE STARTED " +
   STRING(today,"99-99-99") + " " + STRING(time,"HH:MM:SS") + ")" SIDE-LABELS
   FRAME clog.

FUNCTION fBCopy RETURNS LOGICAL:
   
   DEF VAR llSaved    AS LOG  NO-UNDO.
   
   if liCDRLoop mod 1 = 0 THEN DO:
      DISP
         "PORT:" + STRING(iiPort) + " MSC:" + lcmscid  @ cdrfile 
         ttEDR.Datest @ ldaFrmDate
         STRING(ttEDR.Timest,"hh:mm:ss") @ lcFrmTime   
         liCDRLoop           
         liTimeout
      WITH FRAME CLOG.
      PAUSE 0 . 
   END.

   /* if this is an old cdr that came in late then check if it should be 
      saved to old db, otherwise or if this fails then save to current db */
   llSaved = FALSE.
   IF ttEDR.DateSt < TODAY THEN DO:
      llSaved = fSaveCDR2OldDb("PrepEDR",
                               lhttCall). 
   END.

   IF NOT llSaved THEN DO:
      CREATE prepedr.PrepEDR.
      BUFFER-COPY ttEDR TO PrepEDR. 
   END.
   
   llSaved = FALSE.
   IF ttEDR.DateSt < TODAY THEN DO:
      llSaved = fSaveDetail2OldDb("EDRDtl",
                                  ttEDR.DateSt,
                                  ttEDR.DtlSeq,
                                  lcVersion,
                                  lcCDR).
      
   END.

   IF NOT llSaved THEN DO:
      CREATE prepedr.EDRDtl.
      ASSIGN 
         EDRDtl.Datest  = ttEDR.Datest 
         EDRDtl.dtlseq  = ttEDR.DtlSeq
         EDRDtl.Version = lcVersion
         EDRDtl.Detail  = lcCDR. 
   END.
   
   RELEASE prepedr.prepEDR.

   RETURN ERROR-STATUS:ERROR.
END.

VIEW FRAME clog . pause 0.
DISP " PORT " + STRING(iiport) + " WAITING..." @ cdrfile WITH FRAME cLOG.

ehto = 3.
RUN Syst/ufkey.p.
RUN Syst/ufxkey.p(8,3).

IF NOT llOL THEN INPUT STREAM sCDR FROM VALUE(icFileName).
   
EDR_LOOP:
DO WHILE TRUE WITH FRAME clog:  
   
   ASSIGN 
      lcCDRType = ""
      lcVersion = "".

   if llOl then callrec = readOnline (iiPort , lipause).
   ELSE DO:

      import stream sCDR UNFORMATTED callrec NO-ERROR.

      liFileAmt = liFileAmt + 1.
      
      if error-status:error  then do:
          message "File reading is finished !" view-as alert-box
          title " CDRs from file ".
          leave EDR_LOOP.
       end.
   
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

      IF llLeave THEN LEAVE EDR_LOOP.
      ELSE            NEXT  EDR_LOOP.

   END.
   
   fKeepAlive("EDR:Prepaid").
   
   IF callrec eq "*TIMEOUT" THEN DO:
      liTimeout = liTimeout + 1.
      NEXT EDR_LOOP.
   END.

   IF callrec begins "#" THEN DO:
      IF lcErrorLogFile > "" THEN DO:
         OUTPUT STREAM sErrorFile to VALUE(lcErrorLogFile) APPEND.
         PUT STREAM sErrorFile UNFORMATTED fTS2HMS(fMakeTS()) ": " callrec SKIP.
         OUTPUT STREAM sErrorFile CLOSE.
      END.
      NEXT EDR_LOOP.
   END.
      
   etime(true).

   DO liLoop = 1 TO NUM-ENTRIES(callrec,"|"):

      ASSIGN
         lcTemp = ENTRY(liLoop,callrec,"|")
         lcTemp = TRIM(lcTemp).
         
      IF liLoop = 1 THEN lcCDR = lcTemp.
      ELSE               lcCDR = lcCDR + "|" + lcTemp.

   END.
   
   EMPTY TEMP-TABLE ttEDR. 

   DO TRANSACTION:

      ASSIGN
         lcVersion = ENTRY(ttCSVPos.FormatVersion,lcCDR,"|")
         lcCDRType = lcVersion + ENTRY(ttCSVPos.RecordType,lcCDR, "|").

      IF lcCDRType NE lcCsvVersion THEN DO:
         MESSAGE 
            SUBST("Unknown CDR format &1 (cdr-ticket no: &2)",
            lcCDRType,
            ENTRY(fGetPosition(lcCsvVersion,"Running index"),callrec,"|"))
         VIEW-AS ALERT-BOX.
         NEXT.
      END.

      CREATE ttEDR.
      ASSIGN
         ttEDR.CLI         = ENTRY(ttCSVPos.SubscriberNumber,lcCDR,"|")
         ttEDR.NewSC       = INT(ENTRY(ttCSVPos.NewServiceClass,lcCDR,"|"))
         ttEDR.SuccessCode = INT(ENTRY(ttCSVPos.SuccessCode,lcCDR,"|"))
         lcStartDate       = ENTRY(ttCSVPos.EventDate,lcCDR,"|")
         lcStartTime       = ENTRY(ttCSVPos.EventTime,lcCDR,"|")
         ttEDR.dtlseq      = fStreamSequence(INPUT ttEDR.datest, liStream)
         ttEDR.ReadInTS    = fMakeTS()
         ttEDR.ReadDate    = TODAY
         ttEDR.ReadTime    = TIME 
         ttEDR.SubscriberFee = DEC(ENTRY(ttCSVPos.SubscriberFee,lcCDR,"|"))
         ttEDR.BalanceAfter = DEC(ENTRY(ttCSVPos.BalanceAfter,lcCDR,"|"))
         ttEDR.ServiceFeeType = ENTRY(ttCSVPos.ServiceFeeType,lcCDR,"|")
         ttEDR.ServFeeExpDateBefore =
            ENTRY(ttCSVPos.ServFeeExpDateBefore,lcCDR,"|")
         lcmscid           = ENTRY(ttCSVPos.SourceName,lcCDR,"|")
         liCDRLoop         = liCDRLoop + 1.

     ASSIGN
         ttEDR.Datest      = DATE(INT(SUBSTR(lcStartDate,5,2)),  /* MONTH */
                                  INT(SUBSTR(lcStartDate,7,2)),  /* DAY   */
                                  INT(SUBSTR(lcStartDate,1,4)))  /* YEAR  */
         ttEDR.TimeStart   =  3600 * INT(SUBSTR(lcStartTime,1,2)) +
                                60 * INT(SUBSTR(lcStartTime,3,2)) +
                                     INT(SUBSTR(lcStartTime,5,2)) NO-ERROR.

      IF ERROR-STATUS:ERROR THEN DO:
         ttEDR.ErrorCode = {&EDR_ERROR_INVALID_DATE_TIME}.
         fBCopy(). 
         NEXT EDR_LOOP.
      END.
   
      RUN pAnalyzeEDR(BUFFER ttEDR).
      
      fBCopy(). 
   END.

   IF NOT AVAIL ttEDR OR ttEDR.ErrorCode > 0 THEN NEXT EDR_LOOP.

   RUN pHandleEDR(BUFFER ttEDR).
   fLogBasic("EDR handling ended:" + string(ttEDR.cli) + ":" + string(etime)).

END.

IF NOT llOL THEN DO:

   INPUT STREAM sCDR CLOSE.
   MESSAGE 
      "File reading is finished ! " + STRING(liFileAmt) + " CDRs"
   VIEW-AS ALERT-BOX TITLE " CDRs from file ".

END.

