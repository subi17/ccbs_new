/* ----------------------------------------------------------------------
  MODULE .......: mnp_retention_file.p
  TASK .........: Creates MNP retention files(s)
  APPLICATION ..: TMS
  AUTHOR .......: anttis
  CREATED ......: 27.04.12
  Version ......: yoigo
----------------------------------------------------------------------- */

{Syst/commpaa.i}
Syst.Var:katun = "Cron".
Syst.Var:gcBrand = "1".

{Func/cparam2.i}
{Func/ftransdir.i}
{Syst/tmsconst.i}
{Func/fcustpl.i}
{Func/penaltyfee.i}
{Func/fgettxt.i}
{Func/fmakesms.i}
{Mnp/mnp.i}
{Func/email.i}

DEF STREAM sExclude.

FUNCTION fGetPenalty RETURN DECIMAL
   (OUTPUT odaEndDate AS DATE):

  DEF VAR ldeCurrPen AS DEC NO-UNDO.
  DEF VAR lcPriceList AS CHAR NO-UNDO.
  DEF VAR ldePrice AS DEC NO-UNDO.

  CONTRACT_LOOP:
   FOR EACH DCCLI WHERE
            DCCLI.Brand = Syst.Var:gcBrand AND
            DCCLI.DCEvent BEGINS "TERM" AND
            DCCLI.MsSeq = Mobsub.Msseq AND
            DCCLI.ValidTo >= TODAY NO-LOCK,
      FIRST DayCampaign WHERE
            DayCampaign.Brand = Syst.Var:gcBrand AND
            DayCampaign.DCEvent = DCCLI.DCEvent AND
            DayCampaign.DCType = {&DCTYPE_DISCOUNT} AND
            DayCampaign.TermFeeCalc > 0 NO-LOCK:

      /* Count possible penalty fee for contract termination */
      lcPriceList = fFeeModelPriceList(MobSub.Custnum,
                                       MobSub.BillTarget,
                                       DayCampaign.TermFeeModel,
                                       TODAY).

      FIND FIRST FMItem NO-LOCK WHERE
                 FMItem.Brand     = Syst.Var:gcBrand       AND
                 FMItem.FeeModel  = DayCampaign.TermFeeModel AND
                 FMItem.PriceList = lcPriceList AND
                 FMItem.FromDate <= TODAY     AND
                 FMItem.ToDate   >= TODAY NO-ERROR.

      IF AVAIL FMItem THEN ldePrice = FMItem.amount.

      IF DCCLI.Amount NE ? THEN ldePrice = DCCLI.Amount.

      /* calculate a factor for the fee (full / proportional) */
      ldeCurrPen = fCalculateFactor(DCCLI.ValidFrom,
                                    DCCLI.RenewalDate,
                                    DCCLI.ValidTo,
                                    DCCLI.ValidToOrig,
                                    TODAY,
                                    DayCampaign.TermFeeCalc).

      odaEndDate = DCCLI.validto.
      RETURN TRUNCATE(ldeCurrPen * ldePrice,0).
   END.

   RETURN 0.

END.

FUNCTION fCheckRetentionRule RETURN LOGICAL
   (BUFFER Mobsub FOR MobSub,
    BUFFER Segmentation FOR Segmentation,
    OUTPUT ocSMSText AS CHAR):

   DEF VAR ldePenalty AS DEC NO-UNDO. 
   DEF VAR ldaEndDate AS DATE NO-UNDO. 
   DEF VAR liPayType AS INT NO-UNDO. 
      
   RULE_LOOP:
   FOR EACH MNPRetentionRule NO-LOCK WHERE
            MNPRetentionRule.Brand = Syst.Var:gcBrand AND
            MNPRetentionRule.ToDate >= TODAY AND
            MNPRetentionRule.FromDate <= TODAY:

      IF MNPRetentionRule.SegmentCode > "" AND 
         Segmentation.SegmentOffer NE MNPRetentionRule.SegmentCode
         THEN NEXT RULE_LOOP.

      IF MNPRetentionRule.CLIType > "" THEN DO:
         
         CASE MNPRetentionRule.CLIType:
            WHEN "CONT" THEN
              liPayType = {&CLITYPE_PAYTYPE_POSTPAID}.
            WHEN "TARJ" THEN
              liPayType = {&CLITYPE_PAYTYPE_PREPAID}.
            OTHERWISE liPayType = 0.
         END.
         
         IF NOT CAN-FIND(FIRST CLIType NO-LOCK WHERE
                               CLIType.CLIType = MobSub.CLIType AND
                               CLIType.PayType = liPayType) THEN
            NEXT RULE_LOOP.
      END.
      
      /* IF MobSub.PayType EQ {&MOBSUB_PAYTYPE_POSTPAID} AND
         (MNPRetentionRule.PenaltyLeft > 0 OR
          MNPRetentionRule.PenaltyMonthsLeft > 0) THEN DO:

         ldePenalty = fGetPenalty(output ldaEndDate).

         IF ldePenalty NE 0 THEN DO:

            IF MNPRetentionRule.PenaltyLeft > 0 AND
               ldePenalty > MNPRetentionRule.PenaltyLeft THEN DO:
               PUT STREAM sExclude UNFORMATTED
                  MobSub.CLI ";R5"
                  SKIP.
               LEAVE RULE_LOOP.
            END.
            
            IF MNPRetentionRule.PenaltyMonthsLeft > 0 THEN DO:
               IF ((ldaEndDate - TODAY) / 30) > 
                  MNPRetentionRule.PenaltyMonthsLeft THEN  NEXT RULE_LOOP.
            END.
         END.
      END. 
      
      IF MNPRetentionRule.ConsumptionAverage > 0 AND
         MNPRetentionRule.ConsumptionAverage > Segmentation.SegmentCons THEN DO:
         IF MNPRetentionRule.CLIType EQ "CONT" THEN
            PUT STREAM sExclude UNFORMATTED
               MobSub.CLI ";F1"
               SKIP.
         ELSE
            PUT STREAM sExclude UNFORMATTED
               MobSub.CLI ";F2"
               SKIP.
         LEAVE RULE_LOOP.
      END. */

      ocSMSText = MNPRetentionRule.SMSText.
      RETURN TRUE.
   END.

   RETURN FALSE.

END.

DEF STREAM sout.

/*ydr-2632*/
DEF TEMP-TABLE ttOperCategory NO-UNDO
   FIELD operators AS CHAR /*operator category*/
   FIELD amount AS INT /*item count for this category*/
   INDEX operators  IS PRIMARY UNIQUE operators.

DEFINE TEMP-TABLE ttData NO-UNDO
   FIELD custnum LIKE MobSub.Custnum 
   FIELD msseq LIKE MobSub.MsSeq 
   FIELD mnpseq LIKE mnpprocess.mnpseq 
   FIELD smstext AS CHAR
   FIELD RetentionPlatform AS CHAR /*1,2,3*/
   FIELD operatorcat AS CHAR /*category of operators */
   FIELD CustCatName LIKE CustCat.CatName
INDEX operatorcat operatorcat   
INDEX RetentionPlatform RetentionPlatform 
INDEX custnum IS PRIMARY UNIQUE custnum mnpseq msseq.

DEF VAR i AS INT NO-UNDO. 
DEF VAR ldeSMSStamp AS DEC NO-UNDO. 
DEF VAR lcStatusCodes AS CHAR NO-UNDO INIT "2,5". 
DEF VAR lcRetentionSMSText AS CHAR NO-UNDO. 
DEF VAR liLoop AS INTEGER NO-UNDO. 
DEF VAR lcRootDir AS CHARACTER NO-UNDO. 
DEF VAR liExcludeOffset AS INT NO-UNDO.
DEF VAR lcRetExcludeFile AS CHAR NO-UNDO.
DEF VAR lcOperCat AS CHAR NO-UNDO.

DEFINE TEMP-TABLE ttMNPRetPlatform NO-UNDO LIKE MNPRetPlatform
   FIELD CasesPerPlatform AS INT
   FIELD RetentionPlatformName AS CHAR
   FIELD RetentionFile AS CHAR.       

lcRootDir = fCParam("MNP","MNPRetention").
IF lcRootDir = ? OR lcRootDir EQ "" THEN DO:
   MESSAGE "Missing dump dir configuration".
   RETURN.
END.

FUNCTION fSelectCategory RETURNS CHAR
   (icOperator AS CHAR):
   DEF VAR lcOperList AS CHAR NO-UNDO.
   DEF VAR liEntries AS INT NO-UNDO.
   DEF VAR j AS INT NO-UNDO.
   FOR EACH ttOperCategory NO-LOCK:
      liEntries = NUM-ENTRIES(ttOperCategory.operators).
      IF liEntries EQ 0 THEN NEXT. /*Empty row.*/
      DO j = 1 TO liEntries:
         IF icOperator MATCHES ENTRY(j,ttOperCategory.operators) THEN DO:
            RETURN ttOperCategory.operators.
         END.
      END.
   END.
   RETURN "". /*default empty, always returning some reasonable value*/
END.

/*Function adds found category amout. Category can be for example
   Movistar
   VODA*
   ...*/
FUNCTION fAddCategoryCount RETURNS INT
   (icCategory AS CHAR,
    iiAmt AS INT):
   FIND FIRST ttOperCategory EXCLUSIVE-LOCK WHERE
              ttOperCategory.operators EQ icCategory NO-ERROR.
   IF AVAIL ttOperCategory THEN DO:
      ttOperCategory.Amount = ttOperCategory.Amount + iiAmt.
      RELEASE ttOperCategory.
   END.
   RETURN 0.
END.

   

/*Function returns ho many cases are in given category */
FUNCTION fGetCaseAmount RETURNS INT
   (icOperators AS CHAR):
   FIND FIRST ttOperCategory NO-LOCK WHERE
              ttOperCategory.operators EQ icOperators NO-ERROR.
   IF AVAIL ttOperCategory THEN RETURN ttOperCategory.amount.

   RETURN 0.
END.

FOR EACH MNPRetPlatForm NO-LOCK WHERE
         MNPRetPlatForm.Brand = Syst.Var:gcBrand AND
         MNPRetPlatForm.Todate >= TODAY AND
         MNPRetPlatForm.FromDate <= TODAY AND
         MNPRetPlatForm.Percentage > 0:

   /*Create category entrioes for dividing MNP rows for the platforms*/
   FIND FIRST ttOperCategory NO-LOCK WHERE
              ttOperCategory.operators EQ MNPRetPlatForm.Operators NO-ERROR.
   IF NOT AVAIL ttOperCategory THEN DO:
      CREATE ttOperCategory.
      ttOperCategory.operators = MNPRetPlatForm.Operators.
   END.
  
   CREATE ttMNPRetPlatform.
   BUFFER-COPY MNPRetPlatform to ttMNPRetPlatform.
   ASSIGN
      ttMNPRetPlatform.RetentionPlatformName = 
         REPLACE(MNPRetPlatForm.Name, " ", "_").
END.


/*Functhin sends email to recipients that are listed in
retention_file.email

*/

FUNCTION fSendRetentionListEmail RETURNS CHAR
   (icFilename AS CHAR):
   DEF VAR lcEmailConfDir AS CHAR NO-UNDO.

   lcEmailConfDir = fCParamC("RepConfDir").
   
   IF icFilename MATCHES "*business*" THEN  
      GetRecipients(lcEmailConfDir + "/mnp_retention_residential_file.email").
   ELSE
      GetRecipients(lcEmailConfDir + "/mnp_retention_file.email").
   
   IF xMailAddr EQ "" THEN RETURN "No address".

   xMailAttach = icFileName.

   IF LOOKUP(lcMailHost,{&HOSTNAME_STAGING}) > 0 THEN DO:
      /*Internal env*/
      SendMaileInvoice("Retention file email", icFilename, "").      
   END.
   ELSE DO:
      /*production*/
      /*SendMail("MNP Retention data", icFilename).*/
      /*SendMail(icFileName,""). to content*/
      SendMail(icFileName,icFileName). /*to content and attachmet*/
   END.
   
END.   
             
FUNCTION fGetOperatorName RETURNS CHAR
   (icOperCode AS CHAR):

   DEF BUFFER MNPOperator FOR MNPOperator.
             
    /*YDR-2632: add operator for making operator based sharing */
   FIND MNPOperator NO-LOCK WHERE
        MNPOperator.Brand = Syst.Var:gcBrand AND
        MNPOperator.OperCode = icOperCode
   NO-ERROR.

   IF AVAIL MNPOperator THEN RETURN MNPOperator.OperName.
   ELSE DO:
      FIND FIRST MNPOperator WHERE
                 MNPOperator.Brand = Syst.Var:gcBrand AND
                 MNPOperator.OperCode = icOperCode
      NO-ERROR.
      IF AVAIL MNPOperator AND
               MNPOperator.OperBrand > ""
      THEN RETURN MNPOperator.OperBrand.
      ELSE RETURN STRING(MNPProcess.OperCode).
   END.
END.


IF NOT CAN-FIND(FIRST MNPRetPlatForm NO-LOCK WHERE
                      MNPRetPlatForm.Brand = Syst.Var:gcBrand AND
                      MNPRetPlatForm.Todate >= TODAY AND
                      MNPRetPlatForm.FromDate <= TODAY AND
                      MNPRetPlatForm.Percentage > 0) THEN DO:
   MESSAGE "Missing retention platform configuration".
   RETURN.
END.

DEF BUFFER bMNPDetails FOR MNPDetails.
DEF BUFFER bMNPProcess FOR MNPProcess.
DEF BUFFER bMNPSub FOR MNPSub.

DEFINE VARIABLE lcbarringstatus AS CHARACTER NO-UNDO INITIAL "Debt_Hotl,Debt_Restricted,Debt_HOTLP".
DEFINE VARIABLE lcResCatNames   AS CHARACTER NO-UNDO INITIAL "RESIDENTIAL NIF,RESIDENTIAL NIE,UNKNOWN CONSUMER,CONSUMER PASSPORT,VIP - MM GROUP EXTERNAL CUSTOMER,SELF EMPLOYEE NIF,SELF EMPLOYEE NIE,DEFAULT CUSTOMERS".
DEFINE VARIABLE lcCustCatName   AS CHARACTER NO-UNDO.
DEFINE TEMP-TABLE b-ttMNPRetPlatform  LIKE ttMNPRetPlatform.
DEF STREAM sout-bus.

lcRetExcludeFile = lcRootDir + "/spool/" + "mnp_retention_exclude_" +
                   STRING(YEAR(TODAY),"9999") + STRING(MONTH(TODAY),"99") + STRING(DAY(TODAY),"99") +
                   "_" + STRING(TIME) + ".txt".

OUTPUT STREAM sExclude TO VALUE(lcRetExcludeFile).

DO liLoop = 1 TO NUM-ENTRIES(lcStatusCodes):

   MNP_LOOP:
   FOR EACH MNPProcess NO-LOCK WHERE
            MNPProcess.Brand = Syst.Var:gcBrand AND
            MNPProcess.MNPType = {&MNP_TYPE_OUT} AND
            MNPProcess.StatusCode = INT(ENTRY(liLoop,lcStatusCodes)),
      FIRST MNPDetails NO-LOCK WHERE
            MNPDetails.mnpseq = MNPProcess.mnpseq:

     IF mnpdetails.statuslimitts < Func.Common:mMakeTS() THEN NEXT MNP_LOOP. 
      /* IF mnpdetails.custidtype EQ "CIF" THEN NEXT MNP_LOOP. 
      Commented out YOT-4095 */
   
      IF MNPProcess.StatusCode EQ {&MNP_ST_ASOL} AND NOT 
         (MNPProcess.StateFlag = {&MNP_STATEFLAG_CONFIRM_PROPOSAL} OR 
          MNPProcess.StateFlag = {&MNP_STATEFLAG_CONFIRM}) THEN NEXT.

      MNP_SUB_LOOP:
      FOR EACH mnpsub NO-LOCK WHERE
               mnpsub.mnpseq = mnpprocess.mnpseq,
         FIRST MobSub NO-LOCK WHERE
               MobSub.MsSeq = MNPSub.MsSeq:
         FIND FIRST Segmentation NO-LOCK WHERE
                    Segmentation.MsSeq = MNPSub.MsSeq NO-ERROR.
         IF AVAILABLE Segmentation THEN DO:
            IF MobSub.PayType = TRUE THEN
            DO:
               PUT STREAM sExclude UNFORMATTED
                        MobSub.CLI ";R1"
                        SKIP.
               NEXT MNP_SUB_LOOP.
            END.
                              
            liExcludeOffset = -720. /* customers active less than 30 days (YDR-2887) */
             
            IF mnpsub.RetentionPlatform > "" THEN NEXT.

            IF MobSub.ActivationTS > Func.Common:mOffSetTS(liExcludeOffSet) THEN DO:
               PUT STREAM sExclude UNFORMATTED
                  MobSub.CLI ";R4"
                  SKIP.
               NEXT  MNP_SUB_LOOP.
            END.                              
            
            FOR EACH Barring NO-LOCK USE-INDEX MsSeq
               WHERE Barring.msseq  = MobSub.msseq
                 AND lookup(Barring.BarringCode,lcbarringstatus) > 0
                 BREAK BY Barring.BarringCode:

               IF FIRST-OF(Barring.BarringCode) AND
                  Barring.BarringStatus = {&BARR_STATUS_ACTIVE} THEN DO:
                  PUT STREAM sExclude UNFORMATTED
                     MobSub.CLI ";R3"
                     SKIP.
                  NEXT MNP_SUB_LOOP.
               END.
            END.
            
            FIND FIRST Customer NO-LOCK
                 WHERE Customer.CustNum = MobSub.CustNum NO-ERROR.
            IF AVAILABLE Customer THEN
            FIND FIRST CustCat NO-LOCK
                 WHERE CustCat.Brand    = Syst.Var:gcBrand
                   AND CustCat.Category = Customer.Category NO-ERROR.
            IF AVAILABLE CustCat THEN 
            DO:
               CASE CustCat.CatName:
                  WHEN "Company CIF" THEN 
                  DO:
                     PUT STREAM sExclude UNFORMATTED
                         MobSub.CLI ";R5"
                     SKIP.
                     NEXT  MNP_SUB_LOOP.
                  END.                     
                  WHEN "Employees - Internal Customer" THEN 
                  DO:
                     PUT STREAM sExclude UNFORMATTED
                         MobSub.CLI ";R2"
                     SKIP.
                     NEXT  MNP_SUB_LOOP.
                  END.                  
               END CASE.
              /* IF LOOKUP(CustCat.CatName,lcResCatNames) = 0 THEN NEXT  MNP_SUB_LOOP. */
               lcCustCatName  =  CustCat.CatName.
            END.
                      
            IF NOT fCheckRetentionRule(BUFFER MobSub, BUFFER Segmentation, OUTPUT lcRetentionSMSText) THEN NEXT.
                                   
            FIND FIRST ttData NO-LOCK WHERE
                       ttData.custnum = MobSub.custnum AND 
                       ttData.msseq = MobSub.msseq AND 
                       ttData.mnpseq = mnpprocess.mnpseq NO-ERROR.
            IF AVAIL ttData THEN NEXT MNP_LOOP.

            CREATE ttData.
            ASSIGN 
               ttData.custnum = MobSub.custnum
               ttData.MsSeq   = MobSub.msseq
               ttData.mnpseq  = mnpprocess.mnpseq
               ttData.smsText = lcRetentionSMSText
               ttData.CustCatName = lcCustCatName
               i = i + 1.
                                                            
            DEF VAR lcOldMNPCat AS CHAR NO-UNDO. 

            MNP_OTHER_LOOP:
            FOR EACH bMNPDetails NO-LOCK WHERE
                     bMNPDetails.CustId = MNPDetails.CustId AND
                     bMNPDetails.DonorCode = "005" AND
                     bMNPDetails.MNPSeq NE MNPDetails.MNPSeq USE-INDEX CustId,
               FIRST bMNPProcess NO-LOCK WHERE
                     bMNPProcess.MNPSeq = bMNPDetails.MNPSeq AND
                     bMNPProcess.MNPType = {&MNP_TYPE_OUT} AND
                     LOOKUP(STRING(bMNPProcess.StatusCode),"2,5") > 0 USE-INDEX MNPSeq,
                EACH bMNPSub NO-LOCK WHERE
                     bMNPSub.MNPSeq = bMNPProcess.MNPSeq AND
                     bMNPSub.RetentionPlatform > "":
               FIND FIRST MNPRetPlatForm NO-LOCK WHERE
                          MNPRetPlatForm.RetentionPlatform = bMNPSub.RetentionPlatform NO-ERROR.
               IF NOT AVAIL MNPRetPlatForm THEN NEXT.

               lcOldMNPCat = fGetOperatorName(bMNPProcess.OperCode).
               lcOldMNPCat = fSelectCategory(lcOldMNPCat).
            
               FIND FIRST ttMNPRetPlatForm NO-LOCK WHERE
                          ttMNPRetPlatForm.Brand = Syst.Var:gcBrand AND
                          ttMNPRetPlatForm.Operators = lcOldMNPCat AND
                          ttMNPRetPlatForm.Name BEGINS MNPRetPlatForm.Name
                          NO-ERROR.
               IF NOT AVAIL ttMNPRetPlatForm THEN NEXT.
               
               ttData.RetentionPlatform = ttMNPRetPlatForm.RetentionPlatform.
               ttData.OperatorCat = lcOldMNPCat.
               fAddCategoryCount(ttData.operatorcat, 1).
               LEAVE MNP_OTHER_LOOP.
            END.
            
            IF ttData.RetentionPlatform EQ "" THEN DO:
               ttData.OperatorCat = fGetOperatorName(MNPProcess.OperCode).
               /*convert the found operator to match category based sharing*/
               ttData.operatorcat = fSelectCategory(ttData.Operatorcat).
               fAddCategoryCount(ttData.operatorcat, 1).
           END.

            IF NOT SESSION:BATCH THEN DO:
               IF i mod 10 = 0 then do:
                  disp i.
                  pause 0.
               end.  
            END.
         END.
      END.
   END.
END.

FOR EACH ttMNPRetPlatform:            
    ttMNPRetPlatform.CasesPerPlatform  =
         fGetCaseAmount(ttMNPRetPlatForm.Operators) *
      (ttMNPRetPlatForm.Percentage / 100).               
END.

/* Allocate data to retention platforms (files) */
DEF VAR liAllocated AS INT NO-UNDO. 
FOR EACH ttMNPRetPlatform NO-LOCK BREAK BY ttMNPRetPlatform.Operators:

   liAllocated = 0.

   FOR EACH ttData WHERE
            ttData.RetentionPlatform = ttMNPRetPlatform.RetentionPlatform:
      liAllocated = liAllocated + 1.
   END.
   
   IF liAllocated NE 0 AND
      liAllocated >= ttMNPRetPlatform.CasesPerPlatform AND
      NOT LAST(ttMNPRetPlatform.Operators) THEN NEXT.

   FOR EACH ttData WHERE
            ttData.Operatorcat = ttMNPRetPlatform.Operators AND
            ttData.RetentionPlatform = "" BREAK BY ttData.Custnum:

      liAllocated = liAllocated + 1.
      ttData.RetentionPlatform = ttMNPRetPlatform.RetentionPlatform.

      IF liAllocated >= ttMNPRetPlatform.CasesPerPlatform AND
         LAST-OF(ttData.Custnum) AND
         NOT LAST-OF(ttMNPRetPlatform.Operators) THEN LEAVE.
   END. 
END.

/* Print configurations to file. ONLY FOR TESTING PURPOSE */
DEF VAR llPrintTest AS LOGICAL NO-UNDO INIT TRUE. /* <-- Set TRUE When testing */

/* TEST Printout all configs */
IF llPrintTest THEN DO:
   DEF STREAM stest.
   OUTPUT STREAM stest TO VALUE("/tmp/mnp_retention_test.txt").

   PUT STREAM stest UNFORMATTED "MNPRetPlatForm" SKIP.
   PUT STREAM stest UNFORMATTED "Brand|FromDate|Name|Percentage|RetentionPlatform|SMSSender|ToDate" SKIP.
   FOR EACH MNPRetPlatForm NO-LOCK:
      PUT STREAM stest UNFORMATTED
         MNPRetPlatForm.Brand "|"
         MNPRetPlatForm.FromDate "|"
         MNPRetPlatForm.Name "|"
         MNPRetPlatForm.Percentage "|"
         MNPRetPlatForm.RetentionPlatform "|"
         MNPRetPlatForm.SMSSender "|"
         MNPRetPlatForm.ToDate SKIP.
   END.

   PUT STREAM stest UNFORMATTED SKIP "ttOperCategory" SKIP.
   PUT STREAM stest UNFORMATTED SKIP "operators|amount" SKIP.
   FOR EACH ttOperCategory NO-LOCK:
      PUT STREAM stest UNFORMATTED
         ttOperCategory.operators "|"
         ttOperCategory.amount SKIP.
   END.

END.

RUN pFileDump.

OUTPUT STREAM sout CLOSE.
OUTPUT STREAM sout-bus CLOSE.

FOR EACH b-ttMNPRetPlatForm NO-LOCK:
   CREATE ttMNPRetPlatForm.
   BUFFER-COPY b-ttMNPRetPlatForm TO ttMNPRetPlatForm.
END.

DEF VAR lcHandledFiles AS CHAR NO-UNDO. 
/* Send email or move files to ongoing directory */
FOR EACH ttMNPRetPlatform NO-LOCK WHERE
         ttMNPRetPlatform.RetentionFile > "":

   /* Results can be written to the same file, do not handle twice */
   IF LOOKUP(ttMNPRetPlatform.RetentionFile,lcHandledFiles) > 0 THEN NEXT.
   lcHandledFiles = (IF lcHandledFiles > "" THEN "," ELSE "") +
                    ttMNPRetPlatform.RetentionFile.
        
   IF ttMNPRetPlatform.RetentionFile MATCHES "*marktel*" AND NOT ttMNPRetPlatform.RetentionFile MATCHES "*business*" THEN DO:
      fSendRetentionListEmail(ttMNPRetPlatform.RetentionFile).
      fMove2TransDir(ttMNPRetPlatform.RetentionFile, "",
                     lcRootDir + "/processed/").
   END.   
   ELSE
   DO:
      IF ttMNPRetPlatform.RetentionFile MATCHES "*business*" THEN 
            fSendRetentionListEmail(ttMNPRetPlatform.RetentionFile).
            
      fMove2TransDir(ttMNPRetPlatform.RetentionFile, "", 
                     lcRootDir + "/outgoing/").      
   END.      
END.

OUTPUT STREAM sExclude CLOSE.

/* TEST Printout all data */
IF llPrintTest THEN DO:
FOR EACH ttMNPRetPlatform:
   PUT STREAM stest UNFORMATTED SKIP "lcSMSSender" SKIP.
   PUT STREAM stest UNFORMATTED SKIP ttMNPRetPlatform.SMSSender SKIP.
   PUT STREAM stest UNFORMATTED SKIP "liCasesPerPlatform" SKIP.
   PUT STREAM stest UNFORMATTED SKIP ttMNPRetPlatform.CasesPerPlatform SKIP.
   PUT STREAM stest UNFORMATTED SKIP "lcRetentionPlatform" SKIP.
   PUT STREAM stest UNFORMATTED SKIP ttMNPRetPlatform.RetentionPlatform SKIP.
   PUT STREAM stest UNFORMATTED SKIP "lcRetentionPlatformName" SKIP.
   PUT STREAM stest UNFORMATTED SKIP ttMNPRetPlatform.RetentionPlatformName SKIP.
   PUT STREAM stest UNFORMATTED SKIP "lcRetentionFile" SKIP.
   PUT STREAM stest UNFORMATTED SKIP ttMNPRetPlatform.RetentionFile SKIP(1).
 end.

   PUT STREAM stest UNFORMATTED SKIP "ttData" SKIP.
   PUT STREAM stest UNFORMATTED SKIP "custnum|msseq|mnpseq|smstext|RetentionPlatform|operatorcat|CategoryName" SKIP.
   FOR EACH ttData NO-LOCK:
      PUT STREAM stest UNFORMATTED
         ttData.custnum "|"
         ttData.msseq "|"
         ttData.mnpseq "|"
         ttData.smstext "|"
         ttData.RetentionPlatform "|"
         ttData.operatorcat "|" 
         ttData.CustCatName SKIP.
   END.
   OUTPUT STREAM stest CLOSE.
END.

IF lcRetExcludeFile > "" THEN
      fMove2TransDir(lcRetExcludeFile, "", lcRootDir + "/outgoing/").

PROCEDURE pFileDump:

   DEF VAR lcDate AS CHARACTER NO-UNDO. 
   DEF VAR i AS INT NO-UNDO. 
   DEF VAR lcOperName AS CHAR NO-UNDO. 
   DEF VAR ldaDueDate AS DATE NO-UNDO. 
   DEF VAR liMaxPeriods AS INT NO-UNDO init 100. 
   DEF VAR liPeriods AS INT NO-UNDO. 
   DEF VAR lcTime AS CHAR NO-UNDO. 
   DEF VAR lcBusinessRetentionFile AS CHAR NO-UNDO.

   lcDate = string(year(today),"9999") + 
            string(month(today),"99") +
            string(day(today),"99").
   lcTime = STRING(TIME).
   
   FOR EACH ttOperCategory NO-LOCK,
       EACH ttMNPRetPlatform WHERE
           ttMNPRetPlatform.Operators = ttOperCategory.Operators:
      
      ttMNPRetPlatform.RetentionFile = lcRootDir + "/spool/" + 
         "mnp_retention_residential" + "_" +
         ttMNPRetPlatform.RetentionPlatformName + "_" + 
         lcDate + "_" + lcTime + ".txt".                
      OUTPUT STREAM sout TO VALUE (ttMNPRetPlatform.RetentionFile) APPEND.
      
      CREATE b-ttMNPRetPlatform.
      BUFFER-COPY ttMNPRetPlatform EXCEPT ttMNPRetPlatform.RetentionFile ttMNPRetPlatform.RetentionPlatform TO b-ttMNPRetPlatform.
      ASSIGN b-ttMNPRetPlatform.RetentionFile      =  lcRootDir + "/spool/" + "mnp_retention_business" + "_" +
                                                      ttMNPRetPlatform.RetentionPlatformName + "_" + lcDate + "_" + lcTime + ".txt"  
             b-ttMNPRetPlatform.RetentionPlatform  =  ttMNPRetPlatform.RetentionPlatform + "Business".      
      OUTPUT STREAM sout-bus TO VALUE (b-ttMNPRetPlatform.RetentionFile) APPEND.      

      FOR EACH ttData NO-LOCK WHERE
               ttData.RetentionPlatform = ttMNPRetPlatform.RetentionPlatform,
         FIRST Customer NO-LOCK WHERE
               Customer.Custnum = ttData.Custnum,
         FIRST MobSub NO-LOCK WHERE
               MobSub.MsSeq = ttData.MsSeq,
         FIRST Segmentation NO-LOCK WHERE
               Segmentation.MsSeq = ttData.MsSeq,
         FIRST MNPProcess NO-LOCK WHERE
               MNPProcess.MNPSeq = ttData.MNPSeq
         BREAK BY ttData.Custnum:

         i = i + 1.

         lcOperName = fGetOperatorName(MNPProcess.OperCode).

         liPeriods = fMNPPeriods  
            (INPUT MNPProcess.CreatedTS,
             INPUT MNPProcess.PortingTime,
             INPUT liMaxPeriods,
             OUTPUT ldaDueDate).
         
         IF LOOKUP(ttData.CustCatName,lcResCatNames) > 0 THEN 
         DO:
            PUT STREAM sout UNFORMATTED 
                  mobsub.cli "|"
                  Func.Common:mTS2HMS(mnpprocess.CreatedTS) "|"
                  Func.Common:mTS2HMS(mnpprocess.portingtime) "|"
                  lcopername "|"
                  mobsub.clitype "|"
                  customer.firstname "|"
                  customer.custname "|"
                  customer.surname2 "|"
                  customer.smsnumber "|"
                  customer.phone "|"
                  Segmentation.SegmentCode "|"
                  ttData.CustCatName "|"
                  (IF liPeriods <= 3 THEN "Y" ELSE "")
                  SKIP.           
         END.
         ELSE
         DO:
            PUT STREAM sout-bus UNFORMATTED 
                  mobsub.cli "|"
                  Func.Common:mTS2HMS(mnpprocess.CreatedTS) "|"
                  Func.Common:mTS2HMS(mnpprocess.portingtime) "|"
                  lcopername "|"
                  mobsub.clitype "|"
                  customer.firstname "|"
                  customer.custname "|"
                  customer.surname2 "|"
                  customer.smsnumber "|"
                  customer.phone "|"
                  Segmentation.SegmentCode "|"
                  ttData.CustCatName "|"
                  (IF liPeriods <= 3 THEN "Y" ELSE "")
                  SKIP.
         END.
                                                                         
         lcRetentionSMSText = fGetSMSTxt(ttData.SMSText,
                                TODAY,
                                Customer.Language,
                                OUTPUT ldeSMSStamp).

         IF lcRetentionSMSText > "" AND
            ttMNPRetPlatform.SMSSender > ""     
         THEN DO:
            lcRetentionSMSText = REPLACE(lcRetentionSMSText,"#SENDER", ttMNPRetPlatform.SMSSender).

            fMakeSchedSMS2(MobSub.CustNum,
                           MobSub.CLI,
                           {&SMSTYPE_MNP_RETENTION},
                           lcRetentionSMSText,
                           ldeSMSStamp,
                           ttMNPRetPlatform.SMSSender,
                           ""). 
         END.
         
         /*mnpprocess already ongoing mark to be it as ongoing*/
         FIND FIRST mnpsub EXCLUSIVE-LOCK WHERE
                    mnpsub.mnpseq = mnpprocess.Mnpseq AND
                    Mnpsub.msseq = mobsub.msseq NO-ERROR.

         IF AVAIL mnpsub THEN DO:
            MNPSub.RetentionPlatform = ttMNPRetPlatform.RetentionPlatform. 
            RELEASE MNPSub.
         END.
      END.   
      OUTPUT STREAM sout close.
      OUTPUT STREAM sout-bus close.
   END. /*Category loop*/
END PROCEDURE. 
