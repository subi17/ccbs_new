/* readtermfile.p      25.03.08/aam

   read subscription terminations from file
*/

{Syst/commali.i}
{Func/cparam2.i}
{Func/fsubstermreq.i}
{Func/msisdn_prefix.i}
{Func/add_lines_request.i}
{Func/fmakemsreq.i}

DEF INPUT  PARAMETER icFile      AS CHAR NO-UNDO.
DEF INPUT  PARAMETER icLogFile   AS CHAR NO-UNDO.
DEF OUTPUT PARAMETER oiRead      AS INT  NO-UNDO.
DEF OUTPUT PARAMETER oiErrors    AS INT  NO-UNDO.

DEF VAR lcLine           AS CHAR NO-UNDO.
DEF VAR liMsSeq          AS INT  NO-UNDO.
DEF VAR lcCLI            AS CHAR NO-UNDO.
DEF VAR lcICC            AS CHAR NO-UNDO.
DEF VAR lcCLIType        AS CHAR NO-UNDO.
DEF VAR lcReason         AS CHAR NO-UNDO.
DEF VAR lcTermDate       AS CHAR NO-UNDO.
DEF VAR ldtTermDate      AS DATE NO-UNDO.
DEF VAR lcTermTime       AS CHAR NO-UNDO.
DEF VAR liTermTime       AS INT  NO-UNDO.
DEF VAR liMaxDates       AS INT  NO-UNDO.
DEF VAR lcMemoTitle      AS CHAR NO-UNDO.
DEF VAR lcMemoTxt        AS CHAR NO-UNDO.
DEF VAR lcTerminationType AS CHAR NO-UNDO.
DEF VAR lcTermType       AS CHAR NO-UNDO.
DEF VAR lcNewCliType     AS CHAR NO-UNDO.
DEF VAR lcSep            AS CHAR NO-UNDO INIT "|".
DEF VAR ldCurrent        AS DEC  NO-UNDO.
DEF VAR lcSource         AS CHAR NO-UNDO.
DEF VAR lcChannel        AS CHAR NO-UNDO.
DEF VAR lcError          AS CHAR NO-UNDO.
DEF VAR lcPlainFile      AS CHAR NO-UNDO.
DEF VAR liCnt            AS INT  NO-UNDO.
DEF VAR liRequest        AS INT  NO-UNDO.
DEF VAR ldKillStamp      AS DEC  NO-UNDO.
DEF VAR liMSISDNStat     AS INT  NO-UNDO.
DEF VAR liSIMStat        AS INT  NO-UNDO.
DEF VAR liQuarantine     AS INT  NO-UNDO.
DEF VAR llPenalty        AS LOG  NO-UNDO.
DEF VAR lcOutOper        AS CHAR NO-UNDO.
DEF VAR llYoigoCLI       AS LOG  NO-UNDO.
DEF VAR llMasmovilCLI    AS LOG  NO-UNDO.
DEF VAR lcTenant         AS CHAR      NO-UNDO.
DEF VAR llYoigoTenant    AS LOGI      NO-UNDO INIT FALSE.
DEF VAR llMasmovilTenant AS LOGI      NO-UNDO INIT FALSE.
DEF VAR liBillPerm       AS INT  NO-UNDO. 
DEF VAR liError          AS INT  NO-UNDO. 

DEF STREAM sRead.
DEF STREAM sLog.

DEF BUFFER OldCliType FOR CliType.

FUNCTION fError RETURNS LOGIC
   (icMessage AS CHAR):
   
   PUT STREAM sLog UNFORMATTED
      lcLine 
      lcSep
      "ERROR:" 
      icMessage
      SKIP.
   
   oiErrors = oiErrors + 1.
     
END FUNCTION.


IF SEARCH(icFile) = ? THEN RETURN "ERROR:File not found".


INPUT STREAM sRead FROM VALUE(icFile).
OUTPUT STREAM sLog TO VALUE(icLogFile) APPEND.

ASSIGN
   ldCurrent  = Func.Common:mMakeTS()
   liMaxDates = fCParamI("SubsTermMaxDates").

IF liMaxDates = ? THEN liMaxDates = 0.
   
PUT STREAM sLog UNFORMATTED
   "File: " icFile
   SKIP
   "Started: " 
   Func.Common:mTS2HMS(ldCurrent)
   SKIP.

ASSIGN
   liCnt       = R-INDEX(icFile,"/")
   lcPlainFile = icFile.
   
IF liCnt > 1 THEN 
   lcPlainFile = SUBSTRING(lcPlainFile,liCnt + 1).


REPEAT:

   IMPORT STREAM sRead UNFORMATTED lcLine.
   
   oiRead  = oiRead + 1.
   
   ASSIGN 
      liMsSeq     = INTEGER(ENTRY(1,lcLine,lcSep))
      lcCLI       = ENTRY(2,lcLine,lcSep)
      lcICC       = ENTRY(3,lcLine,lcSep)
      lcCLIType   = ENTRY(4,lcLine,lcSep)
      lcReason    = ENTRY(5,lcLine,lcSep)
      lcTermDate  = ENTRY(6,lcLine,lcSep)
      lcTermTime  = ENTRY(7,lcLine,lcSep)
      lcMemoTitle = ENTRY(8,lcLine,lcSep)
      lcMemoTxt   = ENTRY(9,lcLine,lcSep) 
      lcTerminationType  = ENTRY(10,lcLine,lcSep)
      lcNewCliType = ENTRY(11,lcLine,lcSep)
      NO-ERROR.
      
   IF ERROR-STATUS:ERROR THEN DO:
      fError("Invalid data format").
      NEXT.
   END.
   
   /* validate reason code */
   IF NOT CAN-FIND(FIRST TMSCodes WHERE
                         TMSCodes.TableName = "MsRequest"  AND
                         TMSCOdes.FieldName = "TermReason" AND
                         TMSCodes.CodeValue = lcReason)
   THEN DO:
      fError("Invalid reason code"). 
      NEXT.
   END.

   liError = fDeleteMsValidation(liMsSeq, INT(lcReason), OUTPUT lcError).
   IF liError NE 0 THEN DO:
      fError(lcError).
      NEXT.
   END.

   FIND FIRST MobSub WHERE MobSub.MsSeq = liMsSeq NO-LOCK NO-ERROR. 

   IF NOT AVAILABLE MobSub THEN DO:
      fError("Subscription not found").
      NEXT.
   END.

       
   IF NOT (MobSub.CLI EQ lcCLI OR MobSub.FixedNumber EQ lcCLI) THEN DO:         
      fError(lcCLI + " not found from subscription").
      NEXT.  
   END.

   IF NOT fIsFixedOnly(lcCLIType) THEN DO:
      IF MobSub.ICC NE lcICC THEN DO:
         fError("Invalid ICC").
         NEXT.
      END.
   END.
   
   IF MobSub.CLIType NE lcCLIType THEN DO:
      fError("Invalid subscription type").
      NEXT.
   END.

   /* Yoigo MSISDN? */
   ASSIGN
      lcTenant         = BUFFER-TENANT-NAME(MobSub)
      llYoigoCLI       = fIsYoigoCLI(Mobsub.CLI)
      llMasmovilCLI    = fIsMasmovilCLI(Mobsub.CLI)
      llYoigoTenant    = (IF lcTenant = {&TENANT_YOIGO}    THEN TRUE ELSE FALSE)  
      llMasmovilTenant = (IF lcTenant = {&TENANT_MASMOVIL} THEN TRUE ELSE FALSE).

   IF (MobSub.PayType = TRUE AND LOOKUP(lcReason,"6,7,8") = 0) OR
      (MobSub.PayType = FALSE AND LOOKUP(lcReason,"4,7,8,10,3,1") = 0) THEN DO:
      fError("Invalid reason code"). 
      NEXT.
   END.

   IF INT(lcReason) = 3 AND ((NOT llYoigoCLI AND llYoigoTenant) OR (NOT llMasmovilCLI AND llMasmovilTenant))THEN DO:
      fError("Cannot choose Order cancellation with MNP numbers!").
      NEXT.
   END.

   fInitialiseValues(
                  INPUT  INT(lcReason),
                  INPUT  llYoigoCLI,
                  INPUT  llMasmovilCLI,
                  OUTPUT liMsisdnStat,
                  OUTPUT liSimStat,
                  OUTPUT liQuarantine).
               
   /* penalty */
   IF MobSub.PayType THEN 
      llPenalty = FALSE. /* prepaid */
   ELSE
      llPenalty = fIsPenalty(INT(lcReason), Mobsub.MsSeq). /* postpaid */


  /* timestamp validation */
   ASSIGN
      ldtTermDate = DATE(INTEGER(SUBSTRING(lcTermDate,5,2)),
                         INTEGER(SUBSTRING(lcTermDate,7,2)),
                         INTEGER(SUBSTRING(lcTermDate,1,4)))
      liTermTime = INTEGER(SUBSTRING(lcTermTime,1,2)) * 3600 +
                   INTEGER(SUBSTRING(lcTermTime,3,2)) * 60   +
                   INTEGER(SUBSTRING(lcTermTime,5,2))
      NO-ERROR.
   
   IF ERROR-STATUS:ERROR OR ldtTermDate = ? THEN DO:
      fError("Invalid termination date / time").
      NEXT.
   END.
   IF ldtTermDate < TODAY THEN DO:
      fError("Termination cannot be dated to past").
      NEXT.
   END.
   IF lcReason = "4" AND ldtTermDate > TODAY THEN DO:
      fError("Termination cannot be scheduled further than " + 
             STRING(0) + " days").
      NEXT.
   END.
   ELSE IF ldtTermDate > TODAY + liMaxDates THEN DO:
      fError("Termination cannot be scheduled further than " + 
             STRING(liMaxDates) + " days").
      NEXT.
   END.
   ASSIGN ldKillStamp = Func.Common:mMake2DT(ldtTermDate,liTermTime).
   fCheckKillTS(INT(lcReason),ldKillStamp, OUTPUT lcError).
   IF lcError NE "" THEN DO:
      fError(lcError).
      NEXT.
   END.

   /* billing permission */
   liBillPerm = fCheckBillingPermission(MobSub.Msseq, OUTPUT lcError).
   
   IF liBillPerm = 1 THEN DO:
      fError(lcError).
      NEXT.
   END.

   ASSIGN lcOutOper   = "".

   /* Check lcTermType IF FULL or Empty -> {&TERMINATION_TYPE_FULL} */
   IF lcTerminationType EQ "FULL" OR 
      lcTerminationType = "" THEN DO:  

      liRequest = fTerminationRequest(MobSub.MSSeq,
                                      ldKillStamp,
                                      liMSISDNStat,
                                      liSIMStat, 
                                      liQuarantine,
                                      INTEGER(llPenalty),
                                      lcOutOper,
                                      lcReason,   /* reason */
                                      "5",        /* source */
                                      "",         /* creator */
                                      0,          /* father request */
                                      {&TERMINATION_TYPE_FULL},
                                      OUTPUT lcError).

      IF liRequest = 0 THEN DO:
         fError("Creation of termination request failed; " + lcError).
         NEXT.
      END.

      fAdditionalLineSTC(liRequest,
                         Func.Common:mMake2DT(ldtTermDate + 1, 0),
                         "DELETE").

      /* Do not create memos for preactivated dummy customer */ 
      IF lcMemoTxt > "" AND MobSub.Custnum NE 233718 THEN DO:
         CREATE Memo.
         ASSIGN 
            Memo.Brand     = Syst.Var:gcBrand
            Memo.HostTable = "Customer"
            Memo.KeyValue  = STRING(MobSub.CustNum)
            Memo.CustNum   = MobSub.CustNum
            Memo.MemoSeq   = NEXT-VALUE(MemoSeq)
            Memo.CreUser   = Syst.Var:katun 
            Memo.MemoTitle = lcMemoTitle
            Memo.MemoText  = "Subsc.ID " + STRING(liMsSeq) +
                             (IF (lcCLI BEGINS "8" OR lcCLI BEGINS "9")
                                 THEN ", MSISDN " + lcCLI + 
                                 ", request " + STRING(liRequest) + CHR(10) + 
                                 lcMemoTxt
                             ELSE ", FixedNumber " + lcCLI + 
                                 ", request " + STRING(liRequest) + CHR(10) + 
                                 lcMemoTxt)
            Memo.CreStamp  = ldCurrent.
      END.
   END.
   ELSE DO:
      IF fIsFixedOnly(lcCLIType) OR
         ( NOT fIsConvergentORFixedOnly(lcCLIType) ) THEN DO:
         fError("Partial termination not allowed for " + lcTerminationType + " only CLIType: " 
                                                       + lcCLIType + " use FULL termination").
         NEXT.
      END.

      IF fIsConvergenceTariff(lcCLIType) AND 
         NOT CAN-FIND(FIRST TermMobSub WHERE TermMobSub.MsSeq = MobSub.MsSeq)
      THEN DO.
         IF lcTerminationType EQ "MOBILE" THEN DO:
            liRequest = fTerminationRequest(MobSub.MSSeq,
                                            ldKillStamp,
                                            liMSISDNStat,
                                            liSIMStat, 
                                            liQuarantine,
                                            INTEGER(llPenalty),
                                            lcOutOper,
                                            lcReason,   /* reason */
                                            "5",        /* source */
                                            "",         /* creator */
                                            0,          /* father request */
                                            {&TERMINATION_TYPE_PARTIAL},
                                            OUTPUT lcError).
       
            IF liRequest = 0 THEN DO:
               fError("Creation of termination request failed; " + lcError).
               NEXT.
            END.
         END.

         ELSE IF lcTerminationType EQ "FIXED" THEN DO:

            IF lcNewCliType = "" THEN DO:
               FIND FIRST OldCliType WHERE
                  OldCliType.Brand   = Syst.Var:gcBrand AND
                  OldCliType.CliType = lcCLIType NO-LOCK NO-ERROR.

               IF LOOKUP(OldCliType.BaseBundle, "DUB,CONT30,CONTS2GB") > 0 THEN 
                  lcNewCliType = "CONT25".
               ELSE IF OldCliType.BaseBundle EQ "CONT32" THEN              
                  lcNewCliType = "CONT33".
               ELSE lcNewCliType = OldCliType.BaseBundle.
            END.
            
            liRequest = fCTChangeRequest(MobSub.msseq,
                                         lcNewCliType,
                                         "",
                                         "",      /* validation is already done in newton */
                                         ldKillStamp,
                                         0,  /* 0 = Credit check ok */
                                         0,
                                         "" /* pcSalesman */,
                                         FALSE,
                                         TRUE,
                                         "",
                                         0,
                                         {&REQUEST_SOURCE_SUBSCRIPTION_TERMINATION}, 
                                         0, /* order id */
                                         liRequest,
                                         "", /*dms: contract_id,channel ->ReqCParam6*/
                                         OUTPUT lcError).

            IF liRequest = 0 THEN DO:
               fError("STC Request creation failed: " +  lcError).
               NEXT.
            END.
         END.
      END.
      ELSE DO:
         fError("Subscription is partially terminated. Use FULL termination.").
         NEXT.
      END.   
      
   END. /* else do */
END.


PUT STREAM sLog UNFORMATTED
   "Ended: " 
   STRING(TODAY,"99.99.9999") " " STRING(TIME,"hh:mm:ss")
   SKIP
   "Read: "
   oiRead 
   " Succesful: "
   oiRead - oiErrors
   " Errors: " 
   oiErrors
   SKIP.
   
   
