/* ----------------------------------------------------------------------
  MODULE .......: read_msisdn_returned.p
  TASK .........: Marks msisdn numbers as returned back to Yoigo
  APPLICATION ..: TMS
  AUTHOR .......: anttis 
  CREATED ......: 02/2010
  Version ......: xfera
----------------------------------------------------------------------- */

{commpaa.i}
katun = "Cron".
gcBrand = "1".

{tmsconst.i}
{ftransdir.i}
{cparam2.i}
{eventlog.i}
{eventval.i}
{email.i}
{msisdn.i}

DEFINE VARIABLE i AS INTEGER NO-UNDO. 
DEFINE VARIABLE lcLine AS CHARACTER NO-UNDO.

DEFINE VARIABLE lcSep AS CHARACTER NO-UNDO INIT ";".
DEFINE VARIABLE lcError AS CHAR NO-UNDO. 
DEFINE VARIABLE lcLogFile AS CHAR NO-UNDO. 

/* files and dirs */
DEFINE VARIABLE lcFileName AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcIncDir  AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcInputFile AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcRootDir AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcProcDir AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcProcessedFile AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcSpoolDir AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcReportFileOut AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcOutDir AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcConfDir AS CHARACTER NO-UNDO. 
DEFINE VARIABLE liNumOK AS INTEGER NO-UNDO. 
DEFINE VARIABLE liNumErr AS INTEGER NO-UNDO. 

/* field variables */
DEFINE VARIABLE lcMSISDN AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcDate AS CHARACTER NO-UNDO. 
DEFINE VARIABLE ldaDate AS DATE NO-UNDO. 

ASSIGN
   lcRootDir = fCParam("MNP","ReturnedMSISDNDir") 
   lcIncDir  = lcRootDir + "incoming/incoming/" 
   lcProcDir = lcRootDir + "incoming/processed/"
   lcSpoolDir = lcRootDir + "outgoing/spool/" 
   lcOutDir = lcRootDir + "outgoing/outgoing/"
   lcConfDir = fCParamC("RepConfDir").

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
INPUT STREAM sFile THROUGH VALUE("ls -1 " + lcIncDir).
REPEAT:

   ASSIGN
      liNumErr = 0
      liNumOK = 0.

   IMPORT STREAM sFile UNFORMATTED lcFileName.
 
   lcInputFile = lcIncDir + lcFileName.
   
   IF SEARCH(lcInputFile) NE ? THEN 
      INPUT STREAM sin FROM VALUE(lcInputFile).
   ELSE NEXT.
   
   fBatchLog("START", lcInputFile).
   lcLogFile = lcSpoolDir + lcFileName + ".log".
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
         lcMSISDN = ENTRY(1,lcLine,lcSep)        
         lcDate = ENTRY(2,lcLine,lcSep)        
         NO-ERROR.
 
      IF ERROR-STATUS:ERROR THEN DO:
         fError("Wrong file format").
         liNumErr = liNumErr + 1 .
         NEXT.
      END.

      ldaDate = date(
         int(substring(lcDate,5,2)),
         int(substring(lcDate,7,2)),
         int(substring(lcDate,1,4))) no-error.
      
      IF ERROR-STATUS:ERROR THEN DO:
         fError("Wrong file format").
         liNumErr = liNumErr + 1 .
         NEXT.
      END.
     
      RUN pReturnMSISDN(lcMSISDN,
                      ldaDate).

      IF RETURN-VALUE BEGINS "ERROR" THEN DO:
         fError(ENTRY(2,RETURN-VALUE,":")).
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

   IF liNumErr > 0 THEN DO:
      /* mail recipients */
      GetRecipients(lcConfDir + "msisdnreturned.email").
      /* send via mail */
      SendMail(lcReportFileOut,"").
   END.

END.

INPUT STREAM sFile CLOSE.

PROCEDURE pReturnMSISDN:

   DEF INPUT PARAMETER icMSISDN AS CHAR NO-UNDO.
   DEF INPUT PARAMETER idaDate AS DATE NO-UNDO.

   FIND FIRST MSISDN WHERE 
       MSISDN.Brand = gcBrand AND
       MSISDN.CLI = icMSISDN NO-LOCK USE-INDEX CLI NO-ERROR.
   IF NOT AVAILABLE MSISDN THEN 
      RETURN "ERROR:MSISDN not available".

   FIND mobsub WHERE
        mobsub.brand = gcBrand AND
        mobsub.cli = msisdn.cli NO-LOCK NO-ERROR.
   IF AVAIL mobsub THEN RETURN "ERROR:MSISDN is in use".
   
   FIND order WHERE
        order.brand = gcBrand AND
        order.cli = msisdn.cli AND
        lookup(order.statuscode,{&ORDER_INACTIVE_STATUSES}) = 0 NO-LOCK NO-ERROR.
   IF AVAIL Order THEN RETURN "ERROR:MSISDN is in ongoing order".

   fMakeMsidnHistory(RECID(MSISDN)).
   
   ASSIGN
      MSISDN.StatusCode = ({&MSISDN_ST_RETURNED_TO_YOIGO})
      MSISDN.Actiondate = TODAY.

   RETURN "".
END.

