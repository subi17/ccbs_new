/* ----------------------------------------------------------------------
  MODULE .......: bob_charge.p
  TASK .........:
  APPLICATION ..: TMS
  AUTHOR .......: Subhash Sanjeevi
  CREATED ......: 27.08.14
  Version ......: Yoigo
----------------------------------------------------------------------- */

/* ***************************  Definitions  ************************** */
{commpaa.i}
katun = "Cron".
gcBrand = "1".

{tmsconst.i}
{ftransdir.i}
{cparam2.i}
{eventlog.i}
{date.i}
{coinv.i}

DEFINE VARIABLE lcLine   AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcSep    AS CHARACTER NO-UNDO INITIAL ";".
DEFINE VARIABLE liNumOK  AS INTEGER   NO-UNDO.
DEFINE VARIABLE liNumErr AS INTEGER   NO-UNDO.

/* files and dirs */
DEFINE VARIABLE lcLogFile       AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcFileName      AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcIncDir        AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcProcessDir    AS CHARACTER No-UNDO.
DEFINE VARIABLE lcInputFile     AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcProcDir       AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcProcessedFile AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcProcessFile   AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcIncProcFile   AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcSpoolDir      AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcReportFileOut AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcOutDir        AS CHARACTER NO-UNDO.
DEFINE VARIABLE ldtConValue     AS DATE      NO-UNDO.
DEFINE VARIABLE liLastDtValue   AS INTEGER   NO-UNDO.
DEFINE VARIABLE liKeyValue      AS INTEGER   NO-UNDO.

/* field variables */
DEFINE VARIABLE lcMSISDN      AS CHARACTER NO-UNDO.
DEFINE VARIABLE liSubID       AS INTEGER   NO-UNDO.
DEFINE VARIABLE ldAmount      AS DECIMAL   NO-UNDO.
DEFINE VARIABLE liDate        AS INTEGER   NO-UNDO.
DEFINE VARIABLE lcBillingItem AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcMemoText    AS CHARACTER NO-UNDO.
DEFINE VARIABLE lDateCr       AS DATE      NO-UNDO.
DEFINE VARIABLE llgFeeModel   AS LOGICAL   NO-UNDO.
DEFINE VARIABLE llgDateValue  AS LOGICAL   NO-UNDO.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */

ASSIGN
   lcIncDir     = fCParam("Charges","IncDir")
   lcProcessDir = fCParam("Charges","IncProcessDir") /* Processing Dir */
   lcProcDir    = fCParam("Charges","IncProcDir")    /* Processed Dir */
   lcSpoolDir   = fCParam("Charges","OutSpoolDir")
   lcOutDir     = fCParam("Charges","OutDir").
   
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
   
   liNumErr = liNumErr + 1.
   
END FUNCTION.


/* File reading and parsing */
INPUT STREAM sFile THROUGH VALUE("ls -1tr " + lcIncDir).
REPEAT:

   IMPORT STREAM sFile UNFORMATTED lcFileName.
 
   lcInputFile = lcIncDir + lcFileName.
   
   IF SEARCH(lcInputFile) NE ? THEN DO:
      IF fCheckFileNameChars(lcFileName) EQ FALSE THEN NEXT.
      
      /* Transfer file from incoming folder to processing folder */
      /* This processing folder is created, to avoid creating 
         duplicate single fee records by reading incoming files */
      lcProcessFile = fMove2TransDir(lcInputFile, "", lcProcessDir).

      IF lcProcessFile EQ "" THEN NEXT.
      ELSE lcIncProcFile = lcProcessDir + lcFileName.
      
      IF NOT SEARCH(lcIncProcFile) EQ ? THEN 
         INPUT STREAM sin FROM VALUE(lcIncProcFile).
   END.
   ELSE NEXT.
   
   ASSIGN
      liNumOk  = 0
      liNumErr = 0.
   
   fBatchLog("START", lcIncProcFile).
   
   lcLogFile = lcSpoolDir + lcFileName + ".log".
   
   OUTPUT STREAM sLog TO VALUE(lcLogFile) APPEND.

   PUT STREAM sLog UNFORMATTED
              lcFilename  " "
              STRING(TODAY,"99.99.99") " "
              STRING(TIME,"hh:mm:ss") SKIP.
  
   RUN ip_CrtSingleFee.
      
   PUT STREAM sLog UNFORMATTED
       "input: " STRING(liNumOK + liNumErr) ", "
       "updated: " STRING(liNumOK) ", "
       "errors: " STRING(liNumErr) SKIP.
   
   INPUT STREAM sin CLOSE.
   OUTPUT STREAM sLog CLOSE.

   lcReportFileOut = fMove2TransDir(lcLogFile, "", lcOutDir).
   lcProcessedFile = fMove2TransDir(lcIncProcFile, "", lcProcDir).
    
   IF lcProcessedFile NE "" THEN fBatchLog("FINISH", lcProcessedFile).
END.

INPUT STREAM sFile CLOSE.

PROCEDURE ip_CrtSingleFee:
    
   LINE_LOOP:
   REPEAT TRANSACTION:
      
      IMPORT STREAM sin UNFORMATTED lcLine.
      IF lcLine EQ "" THEN NEXT.

      /*[Type];[MSISDN];[Subscription ID];[Amount];[Date];[Billing Item];[Memo Text]*/

      ASSIGN
         lcMSISDN       = ENTRY(2,lcLine,lcSep)
         liSubID        = INTEGER(ENTRY(3,lcLine,lcSep))
         ldAmount       = DECIMAL(REPLACE(ENTRY(4,lcLine,lcSep),",","."))
         liDate         = INTEGER(ENTRY(5,lcLine,lcSep))
         lcBillingItem  = ENTRY(6,lcLine,lcSep)
         lcMemoText     = ENTRY(7,lcLine,lcSep)
         NO-ERROR.
 
      IF ERROR-STATUS:ERROR THEN DO:
         fError("Other error").
         NEXT.
      END.

      /* Validating Input Values */
      
      FIND FIRST TermMobSub WHERE
                 TermMobSub.MsSeq = liSubID   AND
                 TermMobSub.CLI   = lcMSISDN  NO-LOCK NO-ERROR.
                 
      IF AVAILABLE TermMobSub THEN DO:
         fError("Non Active Postpaid").
         NEXT.
      END.

      FIND FIRST MobSub WHERE
                 MobSub.MsSeq = liSubID  AND
                 MobSub.CLI   = lcMSISDN NO-LOCK NO-ERROR.
                       
      IF NOT AVAILABLE MobSub THEN DO:
          fError("Invalid Mobile Subscription").
          NEXT.
      END.

      IF MobSub.PayType = TRUE THEN DO:
         fError("Prepaid Subscription").
         NEXT.  
      END.
      
      IF ldAmount <= 0 THEN DO:
         fError("Invalid amount format").
         NEXT.
      END.
         
      IF MONTH(TODAY) = INT(SUBSTRING(STRING(liDate),5,2)) AND
          YEAR(TODAY) = INT(SUBSTRING(STRING(liDate),1,4)) THEN
          ASSIGN ldtConValue   = fInt2Date(liDate,2)
                 liLastDtValue = INTEGER(STRING(YEAR(ldtConValue),"9999") +
                                         STRING(MONTH(ldtConValue),"99")  +
                                         STRING(DAY(ldtConValue),"99")).
      ELSE DO:
         fError("Invalid date").
         NEXT.
      END.   
          
      FIND FIRST BillItem WHERE
                 BillItem.Brand    = gcBrand             AND
                 BillItem.BIGroup  = {&BITEM_GRP_CHARGE} AND
                 Billitem.BillCode = lcBillingItem       NO-LOCK NO-ERROR.
       
      IF NOT AVAILABLE BillItem THEN DO:
         fError("BillItem not available").
         NEXT.
      END.   

      CREATE SingleFee.
      ASSIGN SingleFee.Brand       = gcBrand
             SingleFee.FMItemId    = NEXT-VALUE(bi-seq)
             SingleFee.CustNum     = MobSub.CustNum
             SingleFee.BillTarget  = 1
             SingleFee.CalcObj     = "bob_charge"
             SingleFee.BillCode    = Billitem.BillCode
             SingleFee.BillPeriod  = liDate 
             SingleFee.Concerns[1] = liLastDtValue
             SingleFee.Amt         = ldAmount
             SingleFee.Memo[1]     = ""
             SingleFee.Memo[2]     = ""
             SingleFee.HostTable   = "MobSub"
             SingleFee.KeyValue    = STRING(MobSub.MsSeq)
             SingleFee.BillType    = "CC"
             SingleFee.Contract    = ""
             SingleFee.Active      = TRUE
             SingleFee.FeeModel    = ""
             SingleFee.VATIncl     = FALSE
             liKeyValue            = SingleFee.FMItemId NO-ERROR.
 
       IF NOT ERROR-STATUS:ERROR THEN
          liNumOK = liNumOK + 1.
              
       IF lcMemoText > "" THEN DO:
          CREATE Memo.
          ASSIGN
              Memo.CreStamp  = fMakeTS()
              Memo.Brand     = gcBrand
              Memo.HostTable = "SingleFee"
              Memo.CustNum   = MobSub.CustNum
              Memo.KeyValue  = STRING(liKeyValue)
              Memo.MemoSeq   = NEXT-VALUE(MemoSeq)
              Memo.CreUser   = katun
              Memo.MemoTitle = "Charge"
              Memo.MemoText  = lcMemoText.
      END.
   END.
END PROCEDURE.
    
