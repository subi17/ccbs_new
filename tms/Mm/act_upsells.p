/* ----------------------------------------------------------------------
  module .......: Mm/act_upsell.p
  task .........: Activate upsells from Backdoor tool
  application ..: tms
  author .......: kariaika
  created ......: 25.06.15
  version ......: yoigo
---------------------------------------------------------------------- */

{commpaa.i}
katun = "Cron".
gcBrand = "1".
{tmsconst.i}
{ftransdir.i}
{cparam2.i}
{timestamp.i}
{eventlog.i}
{fmakemsreq.i}
{fbundle.i}
{mdub.i}
{service.i}
{fdss.i}
{fprepaidfee.i}

/* files and dirs */
DEF VAR lcLine           AS CHAR NO-UNDO.
DEF VAR lcLogFile        AS CHAR NO-UNDO. 
DEF VAR lcFileName       AS CHAR NO-UNDO. 
DEF VAR lcIncDir         AS CHAR NO-UNDO. 
DEF VAR lcInputFile      AS CHAR NO-UNDO. 
DEF VAR lcProcDir        AS CHAR NO-UNDO. 
DEF VAR lcProcessedFile  AS CHAR NO-UNDO. 
DEF VAR lcSpoolDir       AS CHAR NO-UNDO. 
DEF VAR lcReportFileOut  AS CHAR NO-UNDO. 
DEF VAR lcOutDir         AS CHAR NO-UNDO. 
DEF VAR lcToday          AS CHAR NO-UNDO.
DEF VAR lcTime           AS CHAR NO-UNDO.
DEF VAR lcSep            AS CHAR NO-UNDO INIT ";".
DEF VAR ldEndDate        AS DATE NO-UNDO.
DEF VAR ldEndStamp       AS DEC  NO-UNDO.
DEF VAR ldeActStamp      AS DEC  NO-UNDO.
DEF VAR ldaActDate       AS DATE NO-UNDO.
DEF VAR ldePMDUBFee      AS DEC  NO-UNDO.
DEF VAR lcBONOContracts  AS CHAR NO-UNDO.
DEF VAR liRead           AS INT NO-UNDO. 
DEF VAR liErrors         AS INT NO-UNDO. 
DEF VAR liRequest        AS INT  NO-UNDO.
DEF VAR lcMemoText       AS CHAR NO-UNDO.

DEF STREAM sin.
DEF STREAM sFile.
DEF STREAM sLog.

ASSIGN
   lcIncDir    = fCParam("UpsellBackTool","IncDir") 
   lcProcDir   = fCParam("UpsellBackTool","IncProcDir")
   lcSpoolDir  = fCParam("UpsellBackTool","OutSpoolDir")
   lcOutDir    = fCParam("UpsellBackTool","OutDir")
   ldePMDUBFee = fgetPrepaidFeeAmount("PMDUB", TODAY)
   lcToday     = STRING(YEAR(TODAY),"9999") + 
                 STRING(MONTH(TODAY),"99")  +
                 STRING(DAY(TODAY),"99")
   lcTime      = REPLACE(STRING(TIME,"hh:mm:ss"),":","")
   lcBONOContracts = fCParamC("BONO_CONTRACTS").


FUNCTION fLogLine RETURNS LOG(icMessage AS CHAR):

   PUT STREAM sLog UNFORMATTED
      lcLine lcSep 
      icMessage SKIP.

END FUNCTION.

/* File reading and parsing */
INPUT STREAM sFile THROUGH VALUE("ls -1tr " + lcIncDir).
REPEAT:

   IMPORT STREAM sFile UNFORMATTED lcFileName. 
   lcInputFile = lcIncDir + lcFileName.

   IF SEARCH(lcInputFile) NE ? THEN DO:
      IF fCheckFileNameChars(lcFileName) EQ FALSE THEN NEXT.
   
      /* To prevent duplicate file handling (YTS-5280) */
      IF CAN-FIND (FIRST ActionLog NO-LOCK WHERE
                         ActionLog.Brand = gcBrand AND
                         ActionLog.TableName = "Cron" AND
                         ActionLog.KeyValue = lcFileName AND
                         ActionLog.ActionID = "upsellBOB" AND
                         ActionLog.ActionStatus = 0) THEN NEXT.

      DO TRANS:
         CREATE ActionLog.
         ASSIGN 
            ActionLog.Brand        = gcBrand   
            ActionLog.TableName    = "Cron"  
            ActionLog.KeyValue     = lcFileName
            ActionLog.ActionID     = "upsellBOB"
            ActionLog.ActionPeriod = YEAR(TODAY) * 100 + 
                                     MONTH(TODAY)
            ActionLog.ActionStatus = 0
            ActionLog.ActionTS     = fMakeTS().
      END.

      INPUT STREAM sin FROM VALUE(lcInputFile).
   END.
   ELSE NEXT.

   lcLogFile = lcSpoolDir + "upsells_" +
               lcToday + "_" + lcTime + ".log".
   OUTPUT STREAM sLog TO VALUE(lcLogFile).
   fBatchLog("START", lcLogFile).

   ASSIGN ldEndDate   = fLastDayOfMonth(TODAY)
          ldEndStamp  = fMake2Dt(ldEndDate,86399)
          ldeActStamp = fMakeTS().

   fSplitTs(ldeActStamp,OUTPUT ldaActDate,OUTPUT liActTime).

   LINE_LOOP:
   REPEAT:
      IMPORT STREAM sin UNFORMATTED lcLine.
      IF lcLine = "" OR lcLine = ? THEN NEXT LINE_LOOP.

      RUN pBobCheckUpsell(INPUT lcLine).
   
      IF RETURN-VALUE BEGINS "ERROR" THEN liErrors = liErrors + 1.
      liRead = liRead + 1.
      
      fLogLine(RETURN-VALUE).

   END. /* REPEAT: LINE_LOOP: */
   
   INPUT STREAM sin CLOSE.
   OUTPUT STREAM sLog CLOSE.
   
   lcReportFileOut = fMove2TransDir(lcLogFile, "", lcOutDir).
   lcProcessedFile = fMove2TransDir(lcInputFile, "", lcProcDir). 
   IF lcProcessedFile NE "" THEN fBatchLog("FINISH", lcProcessedFile).
   
   DO TRANS:
      ASSIGN 
         ActionLog.ActionDec    = liRead
         ActionLog.ActionChar   = "Read: " + STRING(liRead) + 
                                  " Errors: " + STRING(liErrors) + 
                                  " Succesful: " + STRING(liRead - liErrors) + 
                                  CHR(10) + "Finished: " + fTS2HMS(fMakeTS())
         ActionLog.ActionStatus = 3.
   END.
   
END. /* REPEAT: */

INPUT STREAM sFile CLOSE.


PROCEDURE pBobCheckUpsell: 


   DEF INPUT PARAMETER pcLine AS CHAR NO-UNDO. 

   /* local variables */
   DEF VAR lcCLI              AS CHAR NO-UNDO.
   DEF VAR lcUpsell           AS CHAR NO-UNDO.
   DEF VAR lcError            AS CHAR NO-UNDO.
   DEF VAR lcDssId            AS CHAR NO-UNDO. 
   DEF VAR lcAllowedDSS2SubsType AS CHAR NO-UNDO. 
   DEF VAR lcUpSellList          AS CHAR NO-UNDO. 

   IF NUM-ENTRIES(pcLine,lcSep) <> 2 THEN
      RETURN "ERROR:Wrong file format".

   ASSIGN
      lcCLI          = TRIM(ENTRY(1,pcLine,lcSep))
      lcUpsell       = TRIM(ENTRY(2,pcLine,lcSep))
      lcUpSellList   = "DATA6_UPSELL,DSS_UPSELL,DSS2_UPSELL,DSS200_UPSELL,DATA200_UPSELL".

   IF lcUpsell = ? OR 
      LOOKUP(lcUpsell,lcUpSellList) = 0 THEN
      RETURN "ERROR: invalid or missing upsell".

   lcUpsell = UPPER(lcUpsell).

   /* check invoice */
   FIND MobSub WHERE 
        MobSub.Brand = gcBrand AND
        MobSub.CLI   = lcCLI NO-LOCK NO-ERROR.
   IF NOT AVAIL MobSub OR MobSub.PayType = TRUE THEN 
      RETURN "ERROR:TARJ contract or Invalid MSISDN".
      
   lcDssId = fGetActiveDSSId(MobSub.CustNum,fMakeTS()).

   IF lcDssId EQ "DSS" THEN DO:
      IF lcUpsell EQ "DATA6_UPSELL" THEN
         lcUpsell = "DSS_UPSELL".
      ELSE IF lcUpsell EQ "DATA200_UPSELL" THEN 
         lcUpsell = "DSS200_UPSELL".
      ELSE IF lcUpsell NE "DSS_UPSELL"    OR 
              lcUpsell NE "DSS200_UPSELL" THEN
         RETURN "ERROR:Upsell is not DSS compatible".
   END.
   ELSE IF lcDssId EQ "DSS2" THEN DO:
      
      lcAllowedDSS2SubsType = fCParamC("DSS2_SUBS_TYPE").
      
      IF lcUpsell NE "DSS2_UPSELL"    AND
         lcUpsell NE "DATA6_UPSELL"   AND 
         lcUpsell NE "DATA200_UPSELL" THEN
        RETURN "ERROR:Upsell is not DSS2 compatible".
      
      IF LOOKUP(MobSub.CLIType,lcAllowedDSS2SubsType) > 0 THEN DO:
         IF lcUpsell EQ "DATA6_UPSELL" THEN
            lcUpsell = "DSS2_UPSELL".
         ELSE IF lcUpsell EQ "DATA200_UPSELL" THEN 
            lcUpsell = "DSS200_UPSELL".
      END.
      ELSE IF lcUpsell NE "DATA6_UPSELL"   OR 
              lcUpsell NE "DATA200_UPSELL" THEN 
         RETURN "ERROR:Subscription is not DSS2 compatible".
   END.

   IF lcDssId NE "" THEN 
      fCreateUpsellBundle(MobSub.MsSeq,
                          lcUpsell,
                          {&REQUEST_SOURCE_YOIGO_TOOL},
                          fMakeTS(),
                          OUTPUT liRequest,
                          OUTPUT lcError). 
   ELSE RETURN "ERROR: DSS is not active for this subscription".

   IF lcError <> "" THEN
      RETURN lcError.
   lcMemoText = "Ampliación " +  lcUpsell + " - Activar".
   DYNAMIC-FUNCTION("fWriteMemoWithType" IN ghFunc1,
                    "MobSub",                             /* HostTable */
                    STRING(Mobsub.MsSeq),                 /* KeyValue  */
                    MobSub.CustNum,                       /* CustNum   */
                    lcUpsell,                             /* MemoTitle */
                    lcMemoText,                           /* MemoText  */
                    "Service",                            /* MemoType  */
                    katun + "_" + Mobsub.Cli).

   RETURN "OK".

END PROCEDURE.

