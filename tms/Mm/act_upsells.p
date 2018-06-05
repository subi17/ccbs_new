/* ----------------------------------------------------------------------
  module .......: Mm/act_upsell.p
  task .........: Activate upsells from Backdoor tool
  application ..: tms
  author .......: kariaika
  created ......: 25.06.15
  version ......: yoigo
---------------------------------------------------------------------- */

{Syst/commpaa.i}
Syst.Var:katun = "Cron".
Syst.Var:gcBrand = "1".
{Syst/tmsconst.i}
{Func/ftransdir.i}
{Func/cparam2.i}
{Syst/eventlog.i}
{Func/fmakemsreq.i}
{Func/upsellbundle.i}
{Func/mdub.i}
{Func/service.i}
{Func/fdss.i}
{Func/fprepaidfee.i}

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
DEF VAR lcResult         AS CHAR NO-UNDO.  /* YCO-441 */
DEF VAR lcBundleCLITypes AS CHAR NO-UNDO.  /* YCO-457 */

/* YCO-1 List of compatible tariffs */ 
DEF VAR cValidList AS CHAR INITIAL
   "CONT6,CONT7,CONT8,CONT9,CONT15,CONTF11,CONTF20D,CONTF30,CONTF40,CONTF55,CONTF8,CONTM,CONTM2,CONT23,CONT24,CONT25,CONT26,CONTS12,CONTS15,CONTS16,CONTS20,CONTS21,CONTS25,CONTS26,CONTS30,CONTS32,CONT28,CONT27,CONT31,CONTRD1,CONTRD2,CONTRD3,CONTRD4,CONTRD9".

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
   lcBONOContracts = fCParamC("BONO_CONTRACTS")
   lcBundleCLITypes = fCParamC("BUNDLE_BASED_CLITYPES"). /* YCO-457 */


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
                         ActionLog.Brand = Syst.Var:gcBrand AND
                         ActionLog.TableName = "Cron" AND
                         ActionLog.KeyValue = lcFileName AND
                         ActionLog.ActionID = "upsellBOB" AND
                         ActionLog.ActionStatus = 0) THEN NEXT.

      DO TRANS:
         CREATE ActionLog.
         ASSIGN 
            ActionLog.Brand        = Syst.Var:gcBrand   
            ActionLog.TableName    = "Cron"  
            ActionLog.KeyValue     = lcFileName
            ActionLog.ActionID     = "upsellBOB"
            ActionLog.ActionPeriod = YEAR(TODAY) * 100 + 
                                     MONTH(TODAY)
            ActionLog.ActionStatus = 0
            ActionLog.ActionTS     = Func.Common:mMakeTS().
      END.

      INPUT STREAM sin FROM VALUE(lcInputFile).
   END.
   ELSE NEXT.

   lcLogFile = lcSpoolDir + "upsells_" +
               lcToday + "_" + lcTime + ".log".
   OUTPUT STREAM sLog TO VALUE(lcLogFile).
   fBatchLog("START", lcLogFile).

   ASSIGN ldEndDate   = Func.Common:mLastDayOfMonth(TODAY)
          ldEndStamp  = Func.Common:mMake2DT(ldEndDate,86399)
          ldeActStamp = Func.Common:mMakeTS().

   Func.Common:mSplitTS(ldeActStamp,OUTPUT ldaActDate,OUTPUT liActTime).

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
                                  CHR(10) + "Finished: " + Func.Common:mTS2HMS(Func.Common:mMakeTS())
         ActionLog.ActionStatus = 3.
   END.
   
END. /* REPEAT: */

INPUT STREAM sFile CLOSE.


PROCEDURE pBobCheckUpsell: 


   DEF INPUT PARAMETER pcLine AS CHAR NO-UNDO. 

   /* local variables */
   DEF VAR lcCLI              AS CHAR NO-UNDO.
   DEF VAR lcUpsell           AS CHAR NO-UNDO.
   DEF VAR lcSMS              AS CHAR NO-UNDO.
   DEF VAR llSMS              AS LOG  NO-UNDO.
   DEF VAR lcSMS_Text         AS CHAR NO-UNDO.
   DEF VAR lcError            AS CHAR NO-UNDO.
   DEF VAR lcDssId            AS CHAR NO-UNDO. 
   DEF VAR lcAllowedDSS2SubsType AS CHAR NO-UNDO. 
   DEF VAR lcUpSellList          AS CHAR NO-UNDO. 
   DEF VAR lcMemoTitle           AS CHAR NO-UNDO. 
   DEF VAR lcclitype          AS CHAR NO-UNDO. /* YCO-457 */
   
   /* YCO-3 - Santarder upsells are based on a 3 items per line format */
   IF NUM-ENTRIES(pcLine,lcSep) < 2 OR 
      NUM-ENTRIES(pcLine,lcSep) > 3 THEN 
      RETURN "ERROR:Wrong file format".

   /* YCO-3 - Adding 1gb and 5gb upsells to the list */
   /* YCO-441 - Adding 3Gb rentention upsell to the list */
   ASSIGN
      lcCLI          = TRIM(ENTRY(1,pcLine,lcSep))
      lcUpsell       = TRIM(ENTRY(2,pcLine,lcSep))
      lcUpSellList   = "FID3GB_R_UPSELL,SAN1GB_001,SAN5GB_002,DATA6_UPSELL,DSS_UPSELL,DSS2_UPSELL,DSS200_UPSELL,DATA200_UPSELL,FLEX_UPSELL,FLEX_500MB_UPSELL,FLEX_5GB_UPSELL,DSS_FLEX_500MB_UPSELL,DSS_FLEX_5GB_UPSELL".

   IF lcUpsell = ? OR 
      LOOKUP(lcUpsell,lcUpSellList) = 0 THEN
      RETURN "ERROR: invalid or missing upsell".

   IF NUM-ENTRIES(pcLine,lcSep) = 3 THEN 
   DO:
      ASSIGN 
         lcSMS = TRIM(ENTRY(3,pcLine,lcSep))
         llSMS = LOGICAL(lcSMS) NO-ERROR.
         IF ERROR-STATUS:ERROR THEN 
            RETURN "ERROR: invalid SMS value".
            
   END.   
   ELSE 
      llSMS = FALSE.

   lcUpsell = UPPER(lcUpsell).

   /* check invoice */
   FIND MobSub WHERE 
        MobSub.Brand = Syst.Var:gcBrand AND
        MobSub.CLI   = lcCLI NO-LOCK NO-ERROR.
   IF NOT AVAIL MobSub OR MobSub.PayType = TRUE THEN 
      RETURN "ERROR:TARJ contract or Invalid MSISDN".
      
   
   /* YCO-441 Checking Tariff is compatible with 3Gb retention upsell.
              Retention upsell can be activated as long as it is tariff compatible.
              Upsell does not have to be shared in DSS */
   IF lcUpsell = "FID3GB_R_UPSELL" THEN 
   DO:
      /* YCO-457
         - some old legacy tariffs store the tariff in mobsub.tariffbundle rather than mobsub.clitype
         - The compatibility matrix have the specific tariffs rather than the "families" 
           because not all members of a "family" are compatible. So I have to pass the
           tariffbundle that contains the specific tariff to the function in charge of the validation */         

      IF LOOKUP(Mobsub.CliType,lcBundleCLITypes) > 0 THEN 
          lcclitype = Mobsub.tariffbundle.
      ELSE 
          lcclitype = Mobsub.CliType.       
      /* YCO-457 end */
      
      IF fMatrixAnalyse(Syst.Var:gcBrand,
                        "PERCONTR",
                        "PerContract;SubsTypeTo",
                        lcUpsell + ";" + lcclitype, /* YCO-457 */
                        OUTPUT lcResult) NE 1 AND
         ENTRY(1,lcResult,";") NE "?" THEN 
       RETURN "ERROR:Upsell is not compatible with " + MobSub.clitype + " tariff".
   END.
   ELSE
   DO:
      /* Doing usual behaviour before adding the FID3GB_R_UPSELL */   
      lcDssId = fGetActiveDSSId(MobSub.CustNum,Func.Common:mMakeTS()).
 
      IF lcDssID EQ "" AND 
         lcUpsell BEGINS "DSS" THEN 
         RETURN "ERROR: DSS is not active for this subscription".
   
      IF lcDssId EQ "DSS" THEN 
      DO:
         IF lcUpsell EQ "DATA6_UPSELL" OR lcUpsell EQ "FLEX_UPSELL" THEN
            lcUpsell = "DSS_UPSELL".
         ELSE IF lcUpsell EQ "DATA200_UPSELL" THEN 
            lcUpsell = "DSS200_UPSELL".
         ELSE IF lcUpsell EQ "FLEX_500MB_UPSELL" THEN
            lcUpsell = "DSS_FLEX_500MB_UPSELL".
         ELSE IF lcUpsell EQ "FLEX_5GB_UPSELL" THEN
            lcUpsell = "DSS_FLEX_5GB_UPSELL".
         ELSE IF lcUpsell NE "DSS_UPSELL" AND lcUpsell NE "DSS200_UPSELL" THEN
            RETURN "ERROR:Upsell is not DSS compatible".
      END.
      ELSE IF lcDssId EQ "DSS2" THEN
      DO:
      
         lcAllowedDSS2SubsType = fCParamC("DSS2_SUBS_TYPE").
      
         IF lcUpsell NE "DSS2_UPSELL"    AND
            lcUpsell NE "DATA6_UPSELL"   AND 
            lcUpsell NE "DSS200_UPSELL"  AND
            lcUpsell NE "DATA200_UPSELL" AND 
            lcUpsell NE "FLEX_UPSELL"    THEN
            RETURN "ERROR:Upsell is not DSS2 compatible".
      
         IF LOOKUP(MobSub.CLIType,lcAllowedDSS2SubsType) > 0 THEN
         DO:
            IF lcUpsell EQ "DATA6_UPSELL" OR lcUpsell EQ "FLEX_UPSELL" THEN
               lcUpsell = "DSS2_UPSELL".
            ELSE IF lcUpsell EQ "DATA200_UPSELL" THEN 
               lcUpsell = "DSS200_UPSELL".
            ELSE IF lcUpsell EQ "FLEX_500MB_UPSELL" THEN
               lcUpsell = "DSS_FLEX_500MB_UPSELL".
            ELSE IF lcUpsell EQ "FLEX_5GB_UPSELL" THEN
               lcUpsell = "DSS_FLEX_5GB_UPSELL".
         END.
         ELSE IF lcUpsell NE "DATA6_UPSELL"   AND 
                 lcUpsell NE "FLEX_UPSELL"    AND 
                 lcUpsell NE "DATA200_UPSELL" THEN 
                 RETURN "ERROR:Subscription is not DSS2 compatible".
      END.

      /* YCO-1 "1Gb and 5Gb upsell are not compatible with 1.5Gb tariff CONT10."
         In fact, the upsell is not available to all tariffs. 
         As part of Phase I this is the list of available Tariffs. */
      IF (lcUpsell = "SAN1GB_001" OR lcUpsell = "SAN5GB_002") AND  
         LOOKUP(MobSub.clitype,cValidList) = 0 THEN
         RETURN "ERROR:Upsell is not compatible with " + MobSub.clitype + " tariff".

   END.

   fCreateUpsellBundle(MobSub.MsSeq,
                       lcUpsell,
                       {&REQUEST_SOURCE_YOIGO_TOOL}, 
                       Func.Common:mMakeTS(),
                       OUTPUT liRequest,
                       OUTPUT lcError). 

   IF lcError <> "" THEN
      RETURN lcError.

   lcMemoTitle = "".

   /* YDR-2212 New memo text is been added */
   CASE lcUpsell:
      WHEN "DSS_UPSELL" THEN 
         lcMemoTitle = "Data Sharing Service Upsell".
      WHEN "DSS2_UPSELL" THEN 
         lcMemoTitle = "Data Sharing 2 Service Upsell".
      WHEN "DSS200_UPSELL" THEN
         lcMemoTitle = "DSS 200 MB upsell".
      /* YCO-3 Adding memos */
      WHEN "SAN1GB_001" THEN
         lcMemoTitle = "Ampliación 1 GB".
      WHEN "SAN5GB_002" THEN
         lcMemoTitle = "Ampliación 5 GB".
      /* YCO-3 end */
      WHEN "DATA6_UPSELL" THEN 
         lcMemoTitle = "Ampliación 1,5 GB".
      WHEN "DATA200_UPSELL" THEN 
         lcMemoTitle = "DATA 200 MB upsell". 
      WHEN "FLEX_UPSELL" THEN 
         lcMemoTitle = "FLEX upsell".
      WHEN "FLEX_500MB_UPSELL" THEN
         lcMemoTitle = "FLEX 500MB upsell".
      WHEN "FLEX_5GB_UPSELL" THEN
         lcMemoTitle = "FLEX 5GB upsell".    
      /* YCO-276 Adding memos */   
      WHEN "FID3GB_R_UPSELL" THEN   
         lcMemoTitle = "3Gb retention upsell 1 mes".
   END CASE.
      
   /* YCO-4 - Send SMS for SAN1GB_001 and SAN5G_002 activation */
   lcSMS_Text = "".

   IF llSMS THEN DO:
      CASE lcUpsell:
         WHEN "SAN1GB_001" THEN lcSMS_Text = lcUpsell.
         WHEN "SAN5GB_002" THEN lcSMS_Text = lcUpsell.
         OTHERWISE lcSMS_Text = "".
      END.

      IF lcSMS_Text <> "" THEN
          RUN pSendSMS(INPUT MobSub.MsSeq,
                       INPUT 0,
                       INPUT lcSMS_Text,
                       INPUT 10,
                       INPUT {&UPSELL_SMS_SENDER},
                       INPUT "").
   END.

   lcMemoText = "Ampliación " +  lcUpsell + " - Activar".
   Func.Common:mWriteMemoWithType("MobSub",                             /* HostTable */
                    STRING(Mobsub.MsSeq),                 /* KeyValue  */
                    MobSub.CustNum,                       /* CustNum   */
                    lcMemoTitle,                          /* MemoTitle */
                    lcMemoText,                           /* MemoText  */
                    "Service",                            /* MemoType  */
                    Syst.Var:katun + "_" + Mobsub.Cli).

   RETURN "OK".

END PROCEDURE.

