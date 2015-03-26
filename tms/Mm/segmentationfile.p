/* ----------------------------------------------------------------------
  MODULE .......: segmentationfile.p 
  TASK .........: Reads subscription segmentation data files
  APPLICATION ..: TMS
  AUTHOR .......: anttis 
  CREATED ......: 07.08.09
  Version ......: yoigo
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
{timestamp.i}

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {lib/eventlog.i}

   DEFINE VARIABLE lhSegmentation AS HANDLE NO-UNDO.
   lhSegmentation = BUFFER Segmentation:HANDLE.

END.

DEFINE VARIABLE i AS INTEGER NO-UNDO. 
DEFINE VARIABLE lcLine AS CHARACTER NO-UNDO.

DEF VAR lcCli AS CHARACTER NO-UNDO. 
DEF VAR ldaSegmentDate AS DATE NO-UNDO. 
DEF VAR lcSegmentCode AS CHARACTER NO-UNDO. 
DEF VAR lcSegmentCons AS DEC NO-UNDO.
DEF VAR lcSegmentOffer AS CHAR NO-UNDO. 
DEF VAR ldeSegmentCons AS DEC NO-UNDO. 

DEFINE VARIABLE lcSep AS CHARACTER NO-UNDO INIT "|".
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
DEFINE VARIABLE liNumNotUpd AS INTEGER NO-UNDO.


ASSIGN
   lcIncDir    = fCParam("Segmentation","IncomingDir") 
   lcProcDir   = fCParam("Segmentation","IncProcDir")
   lcSpoolDir = fCParam("Segmentation","OutSpoolDir")
   lcOutDir   = fCParam("Segmentation","OutDir")
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

IF llDoEvent THEN RUN StarEventInitialize(lhSegmentation).

/* File reading and parsing */
INPUT STREAM sFile THROUGH VALUE("ls -1tr " + lcIncDir).
REPEAT:

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
         lcCLi = ENTRY(1,lcLine,lcSep)
         ldaSegmentDate = DATE(ENTRY(2,lcLine,lcSep))
         ldeSegmentCons = DEC(ENTRY(3,REPLACE(lcLine,",","."),lcSep))
         lcSegmentCode = TRIM(ENTRY(5,lcLine,lcSep)) 
         lcSegmentOffer = TRIM(ENTRY(8,lcLine,lcSep)) NO-ERROR.


      IF ERROR-STATUS:ERROR THEN DO:
         fError("Wrong file format").
         liNumErr = liNumErr + 1 .
         NEXT.
      END.

      RUN pUpdateSegmentStatus(lcCLi,
                        ldaSegmentDate,
                        lcSegmentCode,
                        lcSegmentOffer,
                        ldeSegmentCons).

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
      GetRecipients(lcConfDir + "segmentationfile.email").
      /* send via mail */
      SendMail(lcReportFileOut,"").
   END.

END.

INPUT STREAM sFile CLOSE.
fCleanEventObjects().

PROCEDURE pUpdateSegmentStatus:

   DEF INPUT PARAMETER icCli AS CHAR NO-UNDO.
   DEF INPUT PARAMETER idaSegmentDate AS DATE NO-UNDO.
   DEF INPUT PARAMETER icSegmentCode AS CHAR NO-UNDO.
   DEF INPUT PARAMETER icSegmentOffer AS CHAR NO-UNDO.
   DEF INPUT PARAMETER ideSegmentConsumption AS DEC NO-UNDO.

   FIND MobSub NO-LOCK WHERE 
        Mobsub.Cli = icCli NO-ERROR.
   IF NOT AVAIL MobSub THEN RETURN "ERROR:Subscription not found".

   FIND FIRST Segmentation NO-LOCK WHERE
              Segmentation.MsSeq = MobSub.MsSeq AND 
              Segmentation.SegmentDate = idaSegmentDate NO-ERROR.
   
   IF NOT AVAIL Segmentation THEN DO:
      CREATE Segmentation.
      ASSIGN
         Segmentation.MsSeq = MobSub.MsSeq
         Segmentation.SegmentCode = icSegmentCode
         Segmentation.SegmentOffer = icSegmentOffer
         Segmentation.SegmentDate = idaSegmentDate
         Segmentation.SegmentCons = ideSegmentConsumption
         Segmentation.SegmentCreation = fMakeTS().
   END.

   /* nothing to do */
   ELSE IF Segmentation.SegmentCode = icSegmentCode AND
           Segmentation.SegmentDate = idaSegmentDate AND
           Segmentation.SegmentOffer = icSegmentOffer AND
           Segmentation.SegmentCons = ideSegmentConsumption THEN 
       RETURN "Nothing to do".
   ELSE DO: /* update will be done */
      FIND CURRENT Segmentation EXCLUSIVE-LOCK.
      IF llDoEvent THEN RUN StarEventSetOldBuffer(lhSegmentation).

      ASSIGN
         Segmentation.SegmentCode = icSegmentCode
         Segmentation.SegmentOffer = icSegmentOffer
         Segmentation.SegmentDate = idaSegmentDate
         Segmentation.SegmentCons = ideSegmentConsumption
         Segmentation.SegmentCreation = fMakeTS().
      IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhSegmentation).
      RELEASE Segmentation.
   END.   
   RETURN "".

END.
