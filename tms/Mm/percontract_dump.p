/* ----------------------------------------------------------------------
  MODULE .......: percontract_dump
  TASK .........: Create a dump file for periodical contracts 
---------------------------------------------------------------------- */

{Syst/commali.i}
{Syst/dumpfile_run.i}
{Func/create_eventlog.i}
{Func/timestamp.i}

DEF INPUT  PARAMETER icDumpID      AS INT  NO-UNDO.
DEF INPUT  PARAMETER icFile        AS CHAR NO-UNDO.
DEF INPUT  PARAMETER icDumpMode    AS CHAR NO-UNDO.
DEF INPUT  PARAMETER idLastDump    AS DEC  NO-UNDO.
DEF INPUT  PARAMETER icEventSource AS CHAR NO-UNDO.
DEF INPUT  PARAMETER icEventFields AS CHAR NO-UNDO.
DEF OUTPUT PARAMETER oiEvents      AS INT  NO-UNDO.
DEF OUTPUT PARAMETER olInterrupted AS LOG  NO-UNDO.

DEF VAR lcNumeric    AS CHAR     NO-UNDO.
DEF VAR lcDelimiter  AS CHAR     NO-UNDO.
DEF VAR lcCreator    AS CHAR     NO-UNDO.
DEF VAR liCnt        AS INT      NO-UNDO.
DEF VAR lcDumpFields AS CHAR     NO-UNDO.
DEF VAR lcValue      AS CHAR     NO-UNDO.
DEF VAR lcField      AS CHAR     NO-UNDO. 
DEF VAR ldaDumpStart AS DATE     NO-UNDO.
DEF VAR ldDumpStart AS DEC      NO-UNDO. 


DEF VAR ldaModified      AS DATE   NO-UNDO.
DEF VAR liTimeMod        AS INT   NO-UNDO.
DEF VAR lhDCCLI          AS HANDLE NO-UNDO.
DEF VAR lhMServLimit     AS HANDLE NO-UNDO.
DEF VAR ldtLastDump      AS DATETIME NO-UNDO.
DEF VAR lcMServFields    AS CHAR   NO-UNDO.
DEF VAR lcMServModFields AS CHAR NO-UNDO.
DEF VAR lcModFields      AS CHAR   NO-UNDO.
DEF VAR lcEventKey       AS CHAR NO-UNDO.
DEF VAR ldEventTS        AS DEC NO-UNDO.  
DEF STREAM sFile.

DEF BUFFER bDCCLI FOR DCCLI. 
DEF BUFFER bMServiceLimit FOR MServiceLimit.

/* temp table for all periodical contracts */
DEFINE TEMP-TABLE ttContract NO-UNDO
   FIELD MsSeq        LIKE DCCLI.MsSeq 
   FIELD CLI          LIKE DCCLI.CLI 
   FIELD Contract     LIKE DCCLI.DCEvent
   FIELD ContractType LIKE DayCampaign.DCType 
   FIELD AgrCust      LIKE MobSub.AgrCust
   FIELD DurType      LIKE DayCampaign.DurType   
   FIELD Renewal      LIKE DayCampaign.Renewal
   FIELD DurUnit      LIKE DayCampaign.DurUnit
   FIELD DurMonths    LIKE DayCampaign.DurMonths 
   FIELD ValidFrom    LIKE MServiceLimit.FromTS
   FIELD ValidTo      LIKE MServiceLimit.EndTS
   FIELD ContractDate LIKE DCCLI.ContractDate
   FIELD RenewalDate  LIKE DCCLI.RenewalDate
   FIELD Active       AS LOG
   FIELD TermDate     LIKE DCCLI.TermDate
   FIELD ContractRowId     AS ROWID
   FIELD CLIType      LIKE MobSub.CliType
   FIELD EventTS      AS DECIMAL
   FIELD Amount       AS DECIMAL.
 
DEF TEMP-TABLE ttPicked NO-UNDO 
   FIELD ContrType AS INT
   FIELD ContrID   AS RECID
   INDEX ContrID ContrType ContrID.

DEF TEMP-TABLE ttTMSCodes NO-UNDO
   LIKE TMSCodes.
   
DEF TEMP-TABLE ttDayCampaign NO-UNDO
   LIKE DayCampaign.

DEF TEMP-TABLE ttServiceLimit NO-UNDO
   LIKE ServiceLimit.
   
DEFINE VARIABLE lcTypeName AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcDurType  AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcRuleName AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcInclUnit AS CHARACTER NO-UNDO. 

FUNCTION fCollectDCCLI RETURNS LOGIC:

   IF CAN-FIND(FIRST ttPicked WHERE 
                     ttPicked.ContrType = 1 AND
                     ttPicked.ContrID   = RECID(DCCLI)) THEN 
      RETURN FALSE.

   CREATE ttPicked.
   ASSIGN 
      ttPicked.ContrType = 1
      ttPicked.ContrID   = RECID(DCCLI).

   RETURN TRUE. 
END FUNCTION.

FUNCTION fCollectMServiceLimit RETURNS LOGIC:

   IF CAN-FIND(FIRST ttPicked WHERE 
                     ttPicked.ContrType = 2 AND
                     ttPicked.ContrID   = RECID(MServiceLimit)) THEN 
      RETURN FALSE.

   CREATE ttPicked.
   ASSIGN 
      ttPicked.ContrType = 2
      ttPicked.ContrID   = RECID(MServiceLimit).

   RETURN TRUE. 
END FUNCTION.


RUN pFillTempTables.

lcNumeric = SESSION:NUMERIC-FORMAT.

FIND FIRST DumpFile WHERE DumpFile.DumpID = icDumpID NO-LOCK NO-ERROR.
IF AVAILABLE DumpFile THEN DO:
   lcDelimiter  = fInitDelimiter(DumpFile.DumpDelimiter).
   
   IF DumpFile.DecimalPoint = "." 
   THEN SESSION:NUMERIC-FORMAT = "AMERICAN".
   ELSE SESSION:NUMERIC-FORMAT = "EUROPEAN".
END.
ELSE DO:
   ASSIGN 
      lcDelimiter = CHR(9)
      SESSION:NUMERIC-FORMAT = "AMERICAN".
END.

FOR EACH DFField OF DumpFile NO-LOCK WHERE
         DFField.ToDate   >= TODAY AND
         DFField.FromDate <= TODAY
BY DFField.OrderNbr:
   lcDumpFields = lcDumpFields + (IF lcDumpFields > "" THEN "," ELSE "") +
                  DFField.DFField.
END.

fSplitTS(idLastDump,
         OUTPUT ldaModified,
         OUTPUT liTimeMod).


ASSIGN
   ldtLastDump = fTimeStamp2DateTime(idLastDump)
   lhDCCLI     = BUFFER DCCLI:HANDLE
   lhMServLimit = BUFFER MServiceLimit:HANDLE
   ldaDumpStart = TODAY - 90
   ldDumpStart = fHMS2TS(ldaDumpStart,"").

OUTPUT STREAM sFile TO VALUE(icFile).

IF icDumpMode = "modified" THEN DO:
   lcMServFields = fEventKeyFields(lhMServLimit).
   lcMServModFields = "SLSeq,DialType,EndTS,FromTS,MsSeq". 
   IF AVAILABLE DumpFile THEN lcModFields = DumpFile.EventLogFields.

   DO liCnt = 1 TO NUM-ENTRIES(icEventSource,"|"):
   
      /* check modification from eventlog */
      IF ENTRY(liCnt,icEventSource,"|") = "EventLog" THEN DO:
      
         RUN pFindFromEventLog(lhDCCLI,
                               ENTRY(liCnt,icEventFields,"|"),
                               lcModFields,
                               idLastDump,
                               "fCollectDCCLI").
 
         RUN pFindFromEventLog(lhMServLimit,
                               lcMServFields,
                               lcMServModFields,
                               idLastDump,
                               "fCollectMServiceLimit").
      END.
      
      /* check modification from a xx field */
      ELSE IF ENTRY(liCnt,icEventSource,"|") = "field" AND
              ENTRY(liCnt,icEventFields,"|") = "xx" 
      THEN DO:
      END.
   END.

   FOR EACH ttPicked WHERE 
            ttPicked.ContrType = 1,
      FIRST DCCLI NO-LOCK WHERE 
            RECID(DCCLI) = ttPicked.ContrID
   BY DCCLI.DCEvent:
      RUN pReadDCCLI.
      RUN pWriteContract.
   END.

   FOR EACH ttPicked WHERE 
            ttPicked.ContrType = 2,
       FIRST MServiceLimit NO-LOCK WHERE 
            RECID(MServiceLimit) = ttPicked.ContrID:
      RUN pReadMServiceLimit.
      RUN pWriteContract.
   END.

END.   /* modified */

ELSE DO: 

   /* DCLI periodical contracts ------------------------------------*/
   DCCLI_loop:
   FOR EACH DCCLI NO-LOCK WHERE
            DCCLI.Brand = gcBrand AND
            DCCLI.ValidTo > TODAY - 90
   ON QUIT UNDO, RETRY
   ON STOP UNDO, RETRY:

      IF RETRY THEN DO:
         olInterrupted = TRUE.
         LEAVE.
      END.

      RUN pReadDCCLI .
      RUN pWriteContract.

   END. /* end DCCLI_loop -----------------------------------------*/


   /* MServiceLimit periodical contracts --------------------------------*/
   MServiceLimit_loop:
   FOR EACH MServiceLimit NO-LOCK WHERE
            MServicelimit.EndTS > ldDumpStart
   ON QUIT UNDO, RETRY
   ON STOP UNDO, RETRY:

      IF RETRY THEN DO:
         olInterrupted = TRUE.
         LEAVE.
      END.

      RUN pReadMServiceLimit.
      RUN pWriteContract.

      IF NOT SESSION:BATCH AND oiEvents MOD 100 = 0 THEN DO:
             PAUSE 0. 
             DISP oiEvents LABEL "Periodical Contracts" 
             WITH OVERLAY ROW 10 CENTERED SIDE-LABELS
             TITLE " Collecting " FRAME fQty.
      END.
    
   END. /* end MServiceLimitLoop ----------------------------------------*/

END.  /* full dump */

IF NOT SESSION:BATCH THEN HIDE FRAME fQty NO-PAUSE.
 
OUTPUT STREAM sFile CLOSE.

SESSION:NUMERIC-FORMAT = lcNumeric.


PROCEDURE pFillTempTables:

   FOR EACH DayCampaign NO-LOCK WHERE
            DayCampaign.Brand = gcBrand:
      CREATE ttDayCampaign.
      BUFFER-COPY DayCampaign TO ttDayCampaign.
   END.

   FOR EACH TMSCodes NO-LOCK WHERE
            TMSCodes.TableName = "DayCampaign" AND
            TMSCodes.InUse > 0:
      CREATE ttTMSCodes.
      BUFFER-COPY TMSCodes TO ttTMSCodes.
   END.
 
   FOR EACH TMSCodes NO-LOCK WHERE
            TMSCodes.TableName = "SLGAanalyse" AND
            TMSCodes.InUse > 0:
      CREATE ttTMSCodes.
      BUFFER-COPY TMSCodes TO ttTMSCodes.
   END.
   
   FOR EACH ServiceLimit NO-LOCK:
      CREATE ttServiceLimit.
      BUFFER-COPY ServiceLimit TO ttServiceLimit.
   END.
   
END PROCEDURE.

PROCEDURE pReadDayCampaign:

      /* Get Type description */
      FIND FIRST ttTMSCodes WHERE
                 ttTMSCodes.Tablename    = "SLGAanalyse"   AND
                 ttTMSCodes.FieldName    = "SLGAType"      AND
                 ttTMSCodes.CodeValue    = ttDayCampaign.DCtype
      NO-LOCK NO-ERROR.
      lcTypeName = (IF AVAIL ttTMSCodes THEN ttTmsCodes.Codename ELSE "").
            
      /* Get Period type description */
      FIND FIRST ttTMSCodes WHERE
                 ttTMSCodes.Tablename    = "Daycampaign"   AND
                 ttTMSCodes.FieldName    = "DurType"       AND
                 ttTMSCodes.CodeValue    = STRING(ttDayCampaign.DurType)
      NO-LOCK NO-ERROR.   
      lcDurType = (IF AVAIL ttTMSCodes THEN ttTmsCodes.Codename ELSE "").
   
      /* Get Renewal rule description */
      FIND FIRST ttTMSCodes WHERE
                 ttTMSCodes.Tablename    = "DayCampaign"   AND
                 ttTMSCodes.FieldName    = "Renewal"       AND
                 ttTMSCodes.CodeValue    = STRING(ttDayCampaign.Renewal)
      NO-LOCK NO-ERROR.   
      lcRuleName = (IF AVAIL ttTMSCodes THEN ttTmsCodes.Codename ELSE "").
   
      /* Get Period unit description */
      FIND FIRST ttTMSCodes WHERE
                 ttTMSCodes.Tablename    = "Daycampaign"   AND
                 ttTMSCodes.FieldName    = "DurUnit"       AND
                 ttTMSCodes.CodeValue    = STRING(ttDayCampaign.DurUnit)
      NO-LOCK NO-ERROR. 
      IF AVAIL ttTMSCodes THEN lcInclUnit = ttTMSCodes.CodeName.

      ASSIGN ttContract.ContractType = ttDayCampaign.DCType 
             ttContract.DurType      = ttDayCampaign.DurType   
             ttContract.Renewal      = ttDayCampaign.Renewal
             ttContract.DurUnit      = ttDayCampaign.DurUnit
             ttContract.DurMonths    = ttDayCampaign.DurMonths.

END PROCEDURE. 

PROCEDURE pReadMobSub:

      FIND MobSub WHERE
           MobSub.MsSeq = ttContract.MsSeq NO-LOCK NO-ERROR.
      IF NOT AVAIL MobSub THEN DO:
          FIND TermMobSub WHERE
               TermMobSub.MSSeq = ttContract.MsSeq NO-LOCK NO-ERROR.
          IF AVAIL TermMobSub THEN 
            ASSIGN ttContract.CLI = TermMobSub.CLI
                   ttContract.AgrCust = TermMobSub.AgrCust
                   ttContract.CLIType = TermMobSub.CLIType.
      END.
      ELSE ASSIGN ttContract.CLI = MobSub.CLI
                  ttContract.AgrCust = MobSub.AgrCust
                  ttContract.CLIType = MobSub.CLIType.

END PROCEDURE.

PROCEDURE pReadDCCLI :

      DEF VAR lcEventKeyFields AS CHARACTER NO-UNDO. 
      lcEventKeyFields = fEventKeyFields(lhDCCLI).

      IF lcEventKeyFields EQ "PercontractID" THEN DO:
         
         lcEventKey = fEventKeyValues(lhDCCLI,lcEventKeyFields).

         FIND FIRST EventLog NO-LOCK USE-INDEX TableName WHERE
                    EventLog.TableName = lhDCCLI:Name AND
                    EventLog.Key       = lcEventKey NO-ERROR.

         IF NOT AVAIL EventLog THEN
            ldEventTS = 0.
         ELSE
            ldEventTS = fHMS2TS(EventLog.EventDate, EventLog.EventTime).
      END.
      ELSE ldEventTS = 0.

      FIND FIRST ttDayCampaign NO-LOCK WHERE
                 ttDayCampaign.Brand   = gcBrand AND
                 ttDayCampaign.DCEvent = DCCLI.DCEvent  NO-ERROR.
      IF NOT AVAIL ttDayCampaign THEN RETURN. 

      CREATE ttContract.
      ASSIGN ttContract.Contract = DCCLI.DCEvent
             ttContract.MsSeq    = DCCLI.MsSeq
             ttContract.ValidFrom = fHMS2TS(DCCLI.ValidFrom,"0")
             ttContract.ValidTo   = fHMS2TS(DCCLI.ValidTo,"0")
             ttContract.TermDate  = DCCLI.TermDate
             ttContract.ContractDate = DCCLI.ContractDate
             ttContract.RenewalDate = DCCLI.RenewalDate
             ttContract.ContractRowId = ROWID(DCCLI)
             ttContract.Active    = (IF DCCLI.ValidTo   >= TODAY  AND
                                       DCCLI.ValidFrom <= TODAY THEN TRUE 
                                    ELSE FALSE)
             ttContract.EventTS = ldEventTS
             ttContract.Amount  = DCCLI.Amount.

      RUN pReadMobSub.
      RUN pReadDayCampaign.
       
END PROCEDURE.

PROCEDURE pReadMServiceLimit :

      DEFINE VARIABLE ldTS AS DECIMAL NO-UNDO. 
      DEFINE VARIABLE ldaServDate AS DATE NO-UNDO.
      DEFINE VARIABLE liServTime AS INTEGER NO-UNDO.
      DEFINE VARIABLE lcSLCode AS CHARACTER NO-UNDO. 

      ldTS = fMakeTS().

      lcEventKey = fGetEventKey(lhMServLimit).
      FIND FIRST EventLog NO-LOCK USE-INDEX TableName WHERE
                 EventLog.TableName = lhMServLimit:Name AND
                 EventLog.Key       = lcEventKey NO-ERROR.

      IF NOT AVAIL EventLog THEN
         ldEventTS = 0.
      ELSE
         ldEventTS = fHMS2TS(EventLog.EventDate, EventLog.EventTime).
      FIND FIRST ttServiceLimit WHERE
                 ttServiceLimit.slseq    = mServiceLimit.slseq AND 
                 ttServiceLimit.dialtype = mServicelimit.dialtype
      NO-LOCK NO-ERROR.
      
      FIND FIRST ttDayCampaign NO-LOCK WHERE
                 ttDayCampaign.Brand   = gcBrand AND
                 ttDayCampaign.DCEvent = ttServiceLimit.GroupCode  NO-ERROR.
      IF NOT AVAIL ttDayCampaign THEN RETURN. 

      CREATE ttContract.

      IF AVAIL ttservicelimit THEN ASSIGN  
         ttContract.Contract = ttservicelimit.groupcode
         lcSLCode   = ttservicelimit.SLCode.
      ELSE RETURN. 

      IF lcSLCode NE "" THEN 
         ttContract.Contract = ttContract.Contract + 
                               "/" + lcSLCode. 

      ASSIGN ttContract.MsSeq    = MServiceLimit.MsSeq
             ttContract.ContractRowId = ROWID(MServiceLimit)
             ttContract.Active   = (IF MServiceLimit.EndTS >= ldTS AND
                                       MServiceLimit.FromTS <= ldTS THEN TRUE
                                       ELSE FALSE)
             ttContract.EventTS = ldEventTS.
      
      fSplitTS(MServiceLimit.FromTS, output ldaServDate, output liServTime).
      ASSIGN ttContract.ValidFrom = MServiceLimit.FromTS
             ttContract.ContractDate = ldaServDate.

      IF MServiceLimit.EndTs >= 99999999 THEN ttContract.ValidTo = fHMS2TS(12/31/2049,"0").
      ELSE 
         ttContract.ValidTo = MServiceLimit.EndTS.
      
      RUN pReadMobSub.
      RUN pReadDayCampaign.        

END PROCEDURE.


PROCEDURE pWriteContract:

      DO liCnt = 1 TO NUM-ENTRIES(lcDumpFields):

         lcField = ENTRY(liCnt,lcDumpFields).
      
         IF lcField BEGINS "#" THEN DO:
            CASE lcField:
            WHEN "#Contract" THEN lcValue = ttContract.Contract.
            WHEN "#SubscriptionId" THEN lcValue =  STRING(ttContract.MsSeq).
            WHEN "#RowId" THEN lcValue = STRING(ttContract.ContractRowId).
            WHEN "#SubscriptionType" THEN lcValue = ttContract.CLIType.
            WHEN "#ContractType" THEN lcValue =  ttContract.ContractType + " " +  lcTypeName.
            WHEN "#MSISDN" THEN lcValue = ttContract.CLI.
            WHEN "#Active" THEN lcValue = STRING(ttContract.Active).
            WHEN "#AgrCustomer" THEN lcValue =  STRING(ttContract.AgrCust).
            WHEN "#PeriodType" THEN lcValue = STRING(ttContract.DurType) + " " + lcDurType. 
            WHEN "#RenewalRule" THEN lcValue = STRING(ttContract.Renewal) + " " + lcRuleName.
            WHEN "#PeriodUnit" THEN lcValue = STRING(ttContract.DurUnit) + " " + lcInclUnit.
            WHEN "#PeriodLenght" THEN lcValue = STRING(ttContract.DurMonths).
            WHEN "#EfectiveDate" THEN lcValue = fTS2HMS(ttContract.ValidFrom).
            WHEN "#ExpirationDate" THEN lcValue = fTS2HMS(ttContract.ValidTo).
            WHEN "#ContractDate" THEN IF ttContract.ContractDate NE ? THEN 
                                         lcValue = STRING(ttContract.ContractDate,"99-99-9999").
                                         ELSE lcValue = "".
            WHEN "#TerminationDate" THEN IF ttContract.TermDate NE ? THEN 
                                            lcValue = STRING(ttContract.TermDate,"99-99-9999"). 
                                            ELSE lcValue = "".
            WHEN "#RenewalDate" THEN IF ttContract.RenewalDate NE ? THEN 
                                        lcValue = STRING(ttContract.RenewalDate,"99-99-9999"). 
                                        ELSE lcValue = "".
            WHEN "#EventTS" THEN lcValue = fTS2HMS(ttContract.EventTS).
            WHEN "#Amount"  THEN IF ttContract.Amount NE ? THEN
                                 lcValue = STRING(ttContract.Amount).
                                 ELSE lcValue = "".
            OTHERWISE lcValue = "".
            END CASE.
         END.
         ELSE lcValue = "". 
  
         PUT STREAM sFile UNFORMATTED lcValue.

         IF liCnt < NUM-ENTRIES(lcDumpFields) THEN 
         PUT STREAM sFile UNFORMATTED lcDelimiter.
      
      END.
    
      PUT STREAM sFile UNFORMATTED  SKIP.
   
      oiEvents = oiEvents + 1.

      DELETE ttContract. 

END PROCEDURE.

