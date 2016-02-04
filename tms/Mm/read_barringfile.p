/* ----------------------------------------------------------------------
  MODULE .......: read_barringfile.p
  TASK .........: Automate barring status update. YDR-119 
  APPLICATION ..: TMS
  AUTHOR .......: anttis 
  CREATED ......: 06/2010
  Version ......: yoigo
----------------------------------------------------------------------- */

{Syst/commpaa.i}
katun = "Cron".
gcBrand = "1".

{Syst/tmsconst.i}
{Func/ftransdir.i}
{Func/cparam2.i}
{Func/lib/eventlog.i}
{Func/barrfunc.i}
{Syst/tmsconst.i}
{Func/tsformat.i}

DEFINE VARIABLE i AS INTEGER NO-UNDO. 
DEFINE VARIABLE lcLine AS CHARACTER NO-UNDO.

DEFINE VARIABLE lcSep AS CHARACTER NO-UNDO INIT ";".
DEFINE VARIABLE lcError AS CHAR NO-UNDO. 
DEFINE VARIABLE lcLogFile AS CHAR NO-UNDO. 

/* files and dirs */
DEFINE VARIABLE lcFileName AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcIncDir  AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcInputFile AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcProcDir AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcProcessedFile AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcSpoolDir AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcReportFileOut AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcOutDir AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcConfDir AS CHARACTER NO-UNDO. 
DEFINE VARIABLE liNumOK AS INTEGER NO-UNDO. 
DEFINE VARIABLE liNumErr AS INTEGER NO-UNDO. 

/* field variables */
DEFINE VARIABLE liMsSeq AS INT NO-UNDO. 
DEFINE VARIABLE lcMSISDN AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcNewBarr AS CHARACTER NO-UNDO. 

ASSIGN
   lcIncDir  = fCParam("BarringFile","IncDir") 
   lcProcDir = fCParam("BarringFile","IncProcDir")
   lcSpoolDir = fCParam("BarringFile","OutSpoolDir")
   lcOutDir   = fCParam("BarringFile","OutDir").

DEF STREAM sin.
DEF STREAM sFile.
DEF STREAM sLog.

FUNCTION fLogLine RETURNS LOGIC
   (icMessage AS CHAR):

   PUT STREAM sLog UNFORMATTED
      lcLine  "|"
      icMessage SKIP.
      
END FUNCTION.

FUNCTION fError RETURNS LOGIC
   (icMessage AS CHAR):

   fLogLine("ERROR:" + icMessage).
END FUNCTION.

/* File reading and parsing */
INPUT STREAM sFile THROUGH VALUE("ls -1tr " + lcIncDir).
REPEAT:

   IMPORT STREAM sFile UNFORMATTED lcFileName.
 
   lcInputFile = lcIncDir + lcFileName.
   
   IF SEARCH(lcInputFile) NE ? THEN 
      INPUT STREAM sin FROM VALUE(lcInputFile).
   ELSE NEXT.

   ASSIGN
      liNumErr = 0
      liNumOK = 0.
   
   fBatchLog("START", lcInputFile).
   lcLogFile = lcSpoolDir + "barring_status_" + ftsformat("yyyymmdd_HHMMss", fMakeTS()) + ".log".
   OUTPUT STREAM sLog TO VALUE(lcLogFile) append.

   PUT STREAM sLog UNFORMATTED
              lcFilename  " "
              STRING(TODAY,"99.99.99") " "
              STRING(TIME,"hh:mm:ss") SKIP.
  
   LINE_LOOP:
   REPEAT:
      
      IMPORT STREAM sin UNFORMATTED lcLine.
      IF lcLine EQ "" THEN NEXT.

      ASSIGN 
         liMsSeq      = INT(ENTRY(1,lcLine,lcSep))
         lcMSISDN     = ENTRY(2,lcLine,lcSep)        
         lcNewBarr    = ENTRY(3,lcLine,lcSep)        
         NO-ERROR.
 
      IF ERROR-STATUS:ERROR THEN DO:
         fError("Wrong file format").
         liNumErr = liNumErr + 1 .
         NEXT.
      END.
     
      RUN pSetBarring(liMsSeq,
                      lcMSISDN,
                      lcNewBarr).

      IF RETURN-VALUE BEGINS "ERROR" THEN DO:
         fError(LEFT-TRIM(RETURN-VALUE,"ERROR:")).
         liNumErr = liNumErr + 1 .
      END.
      ELSE DO:
         liNumOK = liNumOK + 1 .
      END.
   END.
  
   PUT STREAM sLog UNFORMATTED 
       "input: " STRING(liNumOK + liNumErr) ", "
       "updated: " STRING(liNumOK) ", "
       "errors: " STRING(liNumErr) SKIP.

   lcReportFileOut = fMove2TransDir(lcLogFile, "", lcOutDir).
   lcProcessedFile = fMove2TransDir(lcInputFile, "", lcProcDir). 
   IF lcProcessedFile NE "" THEN fBatchLog("FINISH", lcProcessedFile).
   
   INPUT STREAM sin CLOSE.
   OUTPUT STREAM sLog CLOSE.

END.

INPUT STREAM sFile CLOSE.

PROCEDURE pSetBarring:

   DEF INPUT PARAMETER iiMsSeq      AS INT  NO-UNDO.
   DEF INPUT PARAMETER icMSISDN     AS CHAR  NO-UNDO.
   DEF INPUT PARAMETER icBarringList AS CHAR NO-UNDO.

   DEF VAR lcResult  AS CHAR NO-UNDO.
   DEF VAR liRequest AS INT  NO-UNDO.

   FIND MobSub WHERE
        MobSub.MsSeq = iiMsSeq NO-LOCK NO-ERROR.
   IF NOT AVAIL MobSub THEN
      RETURN "ERROR:Subscription not found".

   IF MobSub.CLI NE icMSISDN THEN
      RETURN "ERROR:MSISDN does not match with subscription ID".
   
   IF MobSub.PayType THEN
      RETURN "ERROR:Subscription is prepaid".

   /* create barring request */
   RUN Mm/barrengine.p (iiMsSeq,
                   icBarringList,
                   {&REQUEST_SOURCE_SCRIPT}, /* source  */
                   "", /* creator */
                   fMakeTS(), /* activate */
                   "", /* SMS */
                   OUTPUT lcResult).

   /* another barring request was created after last check */
   IF lcResult = "ONC" THEN RETURN "ERROR:Barring pending".
                               
   liRequest = INTEGER(lcResult) NO-ERROR. 
                               
   IF liRequest > 0 THEN DO:
      
      CREATE Memo.
      ASSIGN 
         Memo.Brand     = gcBrand
         Memo.HostTable = "MobSub"
         Memo.KeyValue  = STRING(MobSub.MsSeq)
         Memo.CustNum   = MobSub.CustNum
         Memo.MemoSeq   = NEXT-VALUE(MemoSeq)
         Memo.CreUser   = "BOB Tool" 
         Memo.MemoType  = "Service"
         Memo.MemoTitle = "Modified barring" 
         Memo.MemoText  = REPLACE(REPLACE(icBarringList,"=0","-released"),
                                  "=1","-applied")
         Memo.CreStamp  = fMakeTS().
      RETURN "OK".
   END.
   ELSE RETURN "ERROR:" + lcResult. 

END.

