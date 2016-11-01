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
DEF VAR lcTempOrderId AS CHAR NO-UNDO.
DEF VAR lcSALOrderId AS CHAR NO-UNDO.
DEF VAR lcFileType AS CHAR NO-UNDO.

DEF VAR llLogWritten AS LOG NO-UNDO INIT FALSE.

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
   llLogWritten = TRUE.   

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

   fBatchLog("START", lcInputFile).
   lcLogFile = lcSpoolDir + "ERR-" + lcFileName.
   OUTPUT STREAM sLog TO VALUE(lcLogFile) append.

   LINE_LOOP:
   REPEAT:

      IMPORT STREAM sin UNFORMATTED lcLine.
      IF lcLine EQ "" THEN NEXT.
      /* Separator ;; cannot be handled with entry, changed to | */
      lcSepLine = REPLACE(lcLine,lcOrigSep,lcSep).

      IF NUM-ENTRIES(lcSepLine,lcSep) NE 44 THEN DO:
         fLogLine("ERROR:Incorrect input data format").
         NEXT.
      END.

      ASSIGN
         lcFileType = TRIM(ENTRY(1,lcSepLine,lcSep))
         lcTempOrderId = TRIM(ENTRY(39,lcSepLine,lcSep))
      NO-ERROR.
      IF lcTempOrderId BEGINS "Y" THEN
         lcYoigoOrderId = SUBSTRING(lcTempOrderId,2).
      ELSE lcYoigoOrderId = lcTempOrderId.
      IF ENTRY(2,lcSepLine,lcSep) BEGINS "Y" THEN
         lcSALOrderId = TRIM(ENTRY(2,lcSepLine,lcSep)).
      ELSE lcSALOrderId = ENTRY(2,lcSepLine,lcSep).

      IF ERROR-STATUS:ERROR THEN DO:
         fLogLine("ERROR:Incorrect input data format").
         NEXT.
      END.

      IF lcFileType NE "SAL" THEN DO:
         fLogLine("ERROR:SAL file expected. Got " + lcFileType).
         NEXT.
      END.
      FIND FIRST Order WHERE
                 Order.brand EQ gcBrand AND
                 Order.orderid EQ INT(lcYoigoOrderId) AND
                 LOOKUP(order.statuscode,{&ORDER_INACTIVE_STATUSES}) = 0 
                 NO-LOCK NO-ERROR.
      IF NOT AVAIL Order THEN DO:
         fLogLine("ERROR:Order " + lcTempOrderId + " not found or " +
                   "is in incorrect status.").
         NEXT.
      END.
      FIND FIRST CliType WHERE
                 Clitype.brand EQ gcBrand AND
                 Clitype.clitype EQ order.clitype NO-LOCK NO-ERROR.
     IF NOT AVAIL Clitype THEN DO:
         fLogLine("ERROR:Order " + lcTempOrderId + " with incorrect clitype " + 
                  order.clitype).
         NEXT.
      END.
      ELSE IF Clitype.fixedlinetype NE {&FIXED_LINE_TYPE_ADSL} THEN DO:
         fLogLine("ERROR:Not ADSL order " + lcTempOrderId).
         NEXT.
      END.
      IF CAN-FIND(FIRST FusionMessage WHERE
                        FusionMessage.orderid EQ INT(lcYoigoOrderId) AND
                        FusionMessage.MessageType EQ 
                           {&FUSIONMESSAGE_TYPE_LOGISTICS} AND 
                        FusionMessage.Source EQ "MasMovil") THEN DO:
         fLogLine("ERROR:Already handled " + lcTempOrderId).
         NEXT.
      END.                  
      RUN pHandleSALFile(lcYoigoOrderId, lcSALOrderid, order.msseq).
   END.

   INPUT STREAM sin CLOSE.
   OUTPUT STREAM sLog CLOSE.

   IF llLogWritten THEN fMove2TransDir(lcLogFile, "", lcOutDir).
   ELSE fMove2TransDir(lcLogFile, "", lcProcDir).
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
      FusionMessage.UpdateTS = FusionMessage.CreatedTS
      FusionMessage.MessageType = {&FUSIONMESSAGE_TYPE_LOGISTICS} 
      FusionMessage.Source = "MasMovil"
      FusionMessage.msseq = iiMsSeq
      FusionMessage.MessageStatus = (IF INDEX(Order.OrderChannel,"pos") > 0
                                     THEN {&FUSIONMESSAGE_STATUS_ONGOING}
                                     ELSE {&FUSIONMESSAGE_STATUS_NEW}).
   RETURN "OK".
END.
