/* ----------------------------------------------------------------------
  MODULE .......: read_salfile_batch.p
  TASK .........: Handles received SAL file from MasMovil
  APPLICATION ..: TMS
  AUTHOR .......: kaaikas
  CREATED ......: 22.09.16
  Version ......: yoigo
----------------------------------------------------------------------- */

{commpaa.i}
katun = "Cron".
gcBrand = "1".

{tmsconst.i}
{ftransdir.i}
{eventlog.i}
{cparam2.i}
{date.i}
{fgettxt.i}
{smsmessage.i}

DEF VAR lcLine AS CHARACTER NO-UNDO.
DEF VAR lcSepLine AS CHARACTER NO-UNDO.
DEF VAR lcSep AS CHARACTER NO-UNDO INIT "|".
DEF VAR lcOrigSep AS CHARACTER NO-UNDO INIT ";;".
DEF VAR liNumOK AS INTEGER NO-UNDO.
DEF VAR liNumErr AS INTEGER NO-UNDO.

/* files and dirs */
DEF VAR lcRootDir AS CHAR NO-UNDO.
DEF VAR lcLogFile AS CHAR NO-UNDO.
DEF VAR lcFileName AS CHARACTER NO-UNDO.
DEF VAR lcIncDir  AS CHARACTER NO-UNDO.
DEF VAR lcInputFile AS CHARACTER NO-UNDO.
DEF VAR lcProcDir AS CHARACTER NO-UNDO.
DEF VAR lcProcessedFile AS CHARACTER NO-UNDO.
DEF VAR lcSpoolDir AS CHARACTER NO-UNDO.
DEF VAR lcOutDir AS CHARACTER NO-UNDO.

/* field variables */
DEF VAR lcYoigoOrderId AS CHAR NO-UNDO.
DEF VAR lcSALOrderId AS CHAR NO-UNDO.
DEF VAR lcFileType AS CHAR NO-UNDO.

ASSIGN
   lcRootDir  = fCParam("MasMovil","RootDir")
   lcIncDir  = lcRootDir + "incoming/incoming/"
   lcProcDir = lcRootDir + "incoming/processed"
   lcSpoolDir = lcRootDir + "outgoing/spool/"
   lcOutDir   = lcRootDir + "outgoing/outgoing/".

DEF STREAM sin.
DEF STREAM sFile.
DEF STREAM sLog.

FUNCTION fLogLine RETURNS LOGIC
   (icMessage AS CHAR):

   PUT STREAM sLog UNFORMATTED
      lcLine  lcOrigSep
      icMessage SKIP.

END FUNCTION.

/* File reading and parsing */
INPUT STREAM sFile THROUGH VALUE("ls -1tr " + lcIncDir).
REPEAT:

   IMPORT STREAM sFile UNFORMATTED lcFileName.

   lcInputFile = lcIncDir + lcFileName.

   IF SEARCH(lcInputFile) NE ? AND lcFileName BEGINS "SAL-" THEN DO:
      IF fCheckFileNameChars(lcFileName) EQ FALSE THEN NEXT.
      INPUT STREAM sin FROM VALUE(lcInputFile).
   END.
   ELSE NEXT.

   ASSIGN
      liNumOk = 0
      liNumErr = 0.

   fBatchLog("START", lcInputFile).
   lcLogFile = lcSpoolDir + lcFileName + ".LOG".
   OUTPUT STREAM sLog TO VALUE(lcLogFile) append.

   /*PUT STREAM sLog UNFORMATTED
              lcFilename  " "
              STRING(TODAY,"99.99.99") " "
              STRING(TIME,"hh:mm:ss") SKIP.
   */

   LINE_LOOP:
   REPEAT:

      IMPORT STREAM sin UNFORMATTED lcLine.
      IF lcLine EQ "" THEN NEXT.
      /* Separator ;; cannot be handled with entry, changed to | */
      lcSepLine = REPLACE(lcLine,lcOrigSep,lcSep).

      IF NUM-ENTRIES(lcSepLine,lcSep) NE 44 THEN DO:
         fLogLine("ERROR:Incorrect input data format").
         liNumErr = liNumErr + 1 .
         NEXT.
      END.

      ASSIGN
         lcFileType = TRIM(ENTRY(1,lcSepLine,lcSep))
         lcSALOrderId = TRIM(ENTRY(2,lcSepLine,lcSep))
         lcYoigoOrderId = TRIM(ENTRY(39,lcSepLine,lcSep))
      NO-ERROR.

      IF ERROR-STATUS:ERROR THEN DO:
         fLogLine("ERROR:Incorrect input data format").
         liNumErr = liNumErr + 1 .
         NEXT.
      END.

      IF lcFileType NE "SAL" THEN DO:
         fLogLine("ERROR:SAL file expected. Got " + lcFileType).
         liNumErr = liNumErr + 1 .
         NEXT.
      END.
      FIND FIRST Order WHERE
                 Order.brand EQ gcBrand AND
                 Order.orderid EQ INT(lcYoigoOrderId) /* AND
                 Order.statuscode EQ*/ NO-LOCK NO-ERROR.
      IF NOT AVAIL Order THEN DO:
         fLogLine("ERROR:Order " + lcYoigoOrderId + " not found or " +
                   "is in incorrect status.").         
         liNumErr = liNumErr + 1 .
         NEXT.
      END.
      FIND FIRST CliType WHERE
                 Clitype.brand EQ gcBrand AND
                 Clitype.clitype EQ order.clitype NO-LOCK NO-ERROR.
     IF NOT AVAIL Clitype THEN DO:
         fLogLine("ERROR:Incorrect clitype " + order.clitype).
         liNumErr = liNumErr + 1 .
         NEXT.
      END.
      ELSE IF Clitype.fixedlinetype NE {&FIXED_LINE_TYPE_ADSL} THEN DO:
         fLogLine("ERROR:Not ADSL order " + lcYoigoOrderId).
         liNumErr = liNumErr + 1 .
         NEXT.
      END.

      RUN pHandleSALFile(lcYoigoOrderId, lcSALOrderid, order.msseq).
      /*fLogLine(RETURN-VALUE).*/
      liNumOK = liNumOK + 1.
   END.

   PUT STREAM sLog UNFORMATTED
       "input: " STRING(liNumOK + liNumErr) ", "
       "updated: " STRING(liNumOK) ", "
       "errors: " STRING(liNumErr) SKIP.

   INPUT STREAM sin CLOSE.
   OUTPUT STREAM sLog CLOSE.

   fMove2TransDir(lcLogFile, "", lcOutDir).
   lcProcessedFile = fMove2TransDir(lcInputFile, "", lcProcDir).
   IF lcProcessedFile NE "" THEN fBatchLog("FINISH", lcProcessedFile).

END.

INPUT STREAM sFile CLOSE.

PROCEDURE pHandleSALfile:

   DEF INPUT PARAMETER icYoigoOrderId AS CHAR NO-UNDO.
   DEF INPUT PARAMETER icSALOrderId AS CHAR NO-UNDO.
   DEF INPUT PARAMETER iiMsSeq AS INT NO-UNDO.

   CREATE FusionMessage.
   ASSIGN
      FusionMessage.AdditionalInfo = icSALOrderId
      FusionMessage.MessageSeq = NEXT-VALUE(FusionMessageSeq)
      FusionMessage.OrderID = INT(icYoigoOrderID)
      FusionMessage.CreatedTS = fMakeTS()
      FusionMessage.MessageID = GUID(GENERATE-UUID)
      FusionMessage.MessageType = "Logistics"
      FusionMessage.Source = "MasMovil"
      FusionMessage.msseq = iiMsSeq.
      
   IF LOOKUP(Order.OrderChannel,"pos") > 0 THEN
      FusionMessage.MessageStatus = {&FUSIONMESSAGE_STATUS_ONGOING}.
   ELSE
      FusionMessage.MessageStatus = {&FUSIONMESSAGE_STATUS_NEW}.
   RETURN "OK".
END.
