/* ----------------------------------------------------------------------
  MODULE .......: brtestcase_collect.p
  TASK .........: Collect test cases for billrun
  APPLICATION ..: tms
  AUTHOR .......: aam
  CREATED ......: 12.03.12
  Version ......: Yoigo
  ---------------------------------------------------------------------- */

DISABLE TRIGGERS FOR LOAD OF FixedFee.
DISABLE TRIGGERS FOR LOAD OF SingleFee.

{Syst/commali.i}
{Func/timestamp.i}
{Func/finvnum.i}
{Syst/funcrunprocess_update.i}
{Func/date.i}
{Syst/tmsconst.i}
{Func/ftaxdata.i}
{Inv/old_unbilled_events.i}
{Inv/billrund.i NEW}

DEF INPUT  PARAMETER iiBRTestQueueID  AS INT  NO-UNDO. 
DEF INPUT  PARAMETER idaPeriodBeg     AS DATE NO-UNDO.
DEF INPUT  PARAMETER idaPeriodEnd     AS DATE NO-UNDO.
DEF INPUT  PARAMETER iiFeePeriod      AS INT  NO-UNDO. 
DEF INPUT  PARAMETER idaInvDate       AS DATE NO-UNDO.
DEF INPUT  PARAMETER idaDueDate       AS DATE NO-UNDO.
DEF INPUT  PARAMETER iiInvType        AS INT  NO-UNDO. 
DEF INPUT  PARAMETER iiFRProcessID    AS INT  NO-UNDO.
DEF INPUT  PARAMETER iiFRExecID       AS INT  NO-UNDO. 
DEF INPUT  PARAMETER iiUpdateInterval AS INT  NO-UNDO.
DEF OUTPUT PARAMETER oiPicked         AS INT  NO-UNDO.

DEF VAR liCheck   AS INT  NO-UNDO.
DEF VAR lhCDR     AS HANDLE NO-UNDO. 
DEF VAR lhCounter AS HANDLE NO-UNDO. 
DEF VAR lhField   AS HANDLE NO-UNDO.
DEF VAR liCustQty AS INT  NO-UNDO. 
DEF VAR ldaOldEventDate  AS DATE NO-UNDO.
DEF VAR liOldEventPeriod AS INT  NO-UNDO.
DEF VAR lcBillRun        AS CHAR NO-UNDO.
DEF VAR lhHandle  AS HANDLE NO-UNDO.

DEF TEMP-TABLE ttCase NO-UNDO
   FIELD BRTestCaseID AS INT
   FIELD CaseQty AS INT 
   FIELD Picked AS INT
   FIELD CaseDone AS LOG 
   INDEX BRTestCaseID BRTestCaseID
   INDEX CaseDone CaseDone.

DEF TEMP-TABLE ttCriteria NO-UNDO
   LIKE BRTestCriteria
   FIELD CheckProc AS CHAR
   FIELD BegDate AS DATE
   FIELD EndDate AS DATE
   FIELD PeriodBeg AS DEC
   FIELD PeriodEnd AS DEC
   INDEX CriteriaTable CriteriaTable CriteriaField
   INDEX BRTestCaseID BRTestCaseID.

DEF TEMP-TABLE ttField NO-UNDO
   FIELD BRTestCaseID AS INT
   FIELD CriteriaTable AS CHAR
   FIELD CriteriaField AS CHAR
   FIELD BegDate AS DATE
   FIELD EndDate AS DATE
   FIELD ValueIncluded AS CHAR
   FIELD ROValueIncluded AS CHAR
   FIELD SingleCDR AS LOG 
   FIELD EventDateTo AS CHAR 
   INDEX CriteriaTable BRTestCaseID CriteriaTable.
   
DEF TEMP-TABLE ttPickCust NO-UNDO
   FIELD BRTestCaseID AS INT
   FIELD InvCust AS INT 
   FIELD CaseList AS CHAR 
   FIELD ErrorMsg AS CHAR
   FIELD InvCreated AS LOG
   INDEX InvCust InvCust
   INDEX InvCreated InvCreated.

DEF TEMP-TABLE ttServiceLimit NO-UNDO
   LIKE ServiceLimit. 
   
FORM
   liCheck  COLON 10 LABEL "Check" SKIP
   oiPicked COLON 10 LABEL "Picked"
WITH OVERLAY SIDE-LABELS ROW 10 CENTERED TITLE " Collect Cases " FRAME fQty.


FUNCTION fGetTaggedDate RETURNS DATE
   (icTaggedDate AS CHAR,
    idaPeriodBeg AS DATE,
    idaPeriodEnd AS DATE):

   DEF VAR ldaResult AS DATE NO-UNDO.
   
   CASE icTaggedDate:
   WHEN "#BillPeriodBeg" THEN ldaResult = idaPeriodBeg.
   WHEN "#BillPeriodEnd" THEN ldaResult = idaPeriodEnd.
   WHEN "#CurrMonthBeg" THEN ldaResult = DATE(MONTH(TODAY),1,YEAR(TODAY)).
   WHEN "#CurrMonthEnd" THEN 
      ldaResult = fLastDayOfMonth(DATE(MONTH(TODAY),1,YEAR(TODAY))).
   WHEN "#PrevMonthBeg" THEN 
      ldaResult = ADD-INTERVAL(DATE(MONTH(TODAY),1,YEAR(TODAY)),-1,"month").
   WHEN "#PrevMonthEnd" THEN ldaResult = DATE(MONTH(TODAY),1,YEAR(TODAY)) - 1.
   WHEN "#CurrDay" OR WHEN "#TODAY" THEN ldaResult = TODAY.
   END CASE.
   
   RETURN ldaResult.
END FUNCTION.

FUNCTION fGetEventDates RETURNS LOGIC
   (icEventDateFrom AS CHAR,
    icEventDateTo AS CHAR,
    idaPeriodBeg AS DATE,
    idaPeriodEnd AS DATE,
    OUTPUT odaBegDate AS DATE,
    OUTPUT odaEndDate AS DATE):
 
   IF icEventDateFrom = "" THEN odaBegDate = idaPeriodBeg.
   ELSE IF INDEX(icEventDateFrom,"#") = 0 THEN 
      odaBegDate = DATE(icEventDateFrom) NO-ERROR.
   ELSE odaBegDate = fGetTaggedDate(icEventDateFrom,
                                    idaPeriodBeg,
                                    idaPeriodEnd).

   IF icEventDateTo = "" THEN odaEndDate = idaPeriodEnd.
   ELSE IF INDEX(icEventDateTo,"#") = 0 THEN 
      odaEndDate = DATE(icEventDateTo) NO-ERROR.
   ELSE odaEndDate = fGetTaggedDate(icEventDateTo,
                                    idaPeriodBeg,
                                    idaPeriodEnd).
    
   RETURN(odaBegDate NE ? AND odaEndDate NE ?). 
   
END FUNCTION.    


/***** Main start *****/

IF NOT SESSION:BATCH THEN DO:
   PAUSE 0.
   VIEW FRAME fQty.
END.

RUN pInitialize.

IF NOT RETURN-VALUE BEGINS "ERROR" THEN 
   RUN pCollect.

IF VALID-HANDLE(lhHandle) THEN 
    RUN pUpdInvGroup in lhHandle.

IF NOT RETURN-VALUE BEGINS "ERROR" THEN
   RUN pSaveResults.
   
IF NOT SESSION:BATCH THEN 
   HIDE FRAME fQty NO-PAUSE.

DELETE OBJECT lhField NO-ERROR.
DELETE OBJECT lhCDR NO-ERROR.
DELETE OBJECT lhCounter NO-ERROR.
DELETE OBJECT lhHandle NO-ERROR.

RETURN RETURN-VALUE.

/***** Main end ******/


PROCEDURE pInitialize:

   FIND FIRST BRTestQueue WHERE BRTestQueue.BRTestQueueID = iiBRTestQueueID
      NO-LOCK NO-ERROR.
   IF NOT AVAILABLE BRTestQueue OR NOT BRTestQueue.Active THEN 
      RETURN "ERROR:Queue not available".
      
   EMPTY TEMP-TABLE ttCase.
   EMPTY TEMP-TABLE ttCriteria.
   
   FOR EACH BRTestQRow OF BRTestQueue NO-LOCK WHERE
            BRTestQRow.Active,
      FIRST BRTestCase NO-LOCK WHERE 
            BRTestCase.BRTestCaseID = BRTestQRow.BRTestCaseID AND
            BRTestCase.Active:

      CREATE ttCase.
      ASSIGN 
         ttCase.BRTestCaseID = BRTestCase.BRTestCaseID
         ttCase.CaseQty = BRTestQRow.CaseQty.
         
      /* criteria for the case */
      FOR EACH BRTestCriteria OF BRTestCase NO-LOCK WHERE
               BRTestCriteria.CriteriaOwner = "BRTestCase" AND 
               BRTestCriteria.Active:

         RUN pInitCriteria(BRTestCase.BRTestCaseID).
         IF RETURN-VALUE BEGINS "ERROR" THEN RETURN RETURN-VALUE.
      END.   
      
      /* criteria for the queue (assign to each case, if not already a case
         level criteria defined) */
      FOR EACH BRTestCriteria OF BRTestQueue NO-LOCK WHERE
               BRTestCriteria.CriteriaOwner = "BRTestQueue" AND 
               BRTestCriteria.Active:

         RUN pInitCriteria(BRTestCase.BRTestCaseID).
         IF RETURN-VALUE BEGINS "ERROR" THEN RETURN RETURN-VALUE.
      END.   
   END.
   
   ASSIGN
      lhCDR = BUFFER MobCDR:HANDLE
      lhCounter = BUFFER InvRowCounter:HANDLE.

   ldaOldEventDate = fOldUnbilledEventLimit(0).
   IF ldaOldEventDate = ? THEN ldaOldEventDate = 1/1/2006.
   liOldEventPeriod = YEAR(ldaOldEventDate) * 100 + MONTH(ldaOldEventDate).

   /* use sequence InvFile to create a unique id for this run */
   lcBillRun = "TEST-BR"  + 
               STRING(NEXT-VALUE(InvFile)) + "_" + 
               STRING(YEAR(TODAY),"9999") + STRING(MONTH(TODAY),"99") +
               STRING(DAY(TODAY),"99") + STRING(TIME,"99999").

   RUN lamupers.p PERSISTENT SET lhHandle.
     
   RETURN "". 
      
END PROCEDURE.

PROCEDURE pInitCriteria:

   DEF INPUT PARAMETER iiBRTestCaseID AS INT  NO-UNDO.

   DEF VAR lcCheckProc AS CHAR NO-UNDO.
    
   lcCheckProc = "pCheck" + BRTestCriteria.CriteriaTable.
   IF LOOKUP(lcCheckProc,THIS-PROCEDURE:INTERNAL-ENTRIES) = 0 THEN
      RETURN "ERROR:Invalid criteria table " + BRTestCriteria.CriteriaTable +
             ", case " + STRING(iiBRTestCaseID).
         
   /* for MobCDR there can be multiple fields defined, for other tables
      only one (constant) key field is available */
   IF BRTestCriteria.CriteriaTable = "MobCDR" THEN DO:
      RUN pInitMobCDR(iiBRTestCaseID,lcCheckProc).
      RETURN RETURN-VALUE.
   END.
   ELSE IF BRTestCriteria.CriteriaTable = "InvRowCounter" THEN DO:
      RUN pInitInvRowCounter(iiBRTestCaseID,lcCheckProc).
      RETURN RETURN-VALUE.
   END.

   IF BRTestCriteria.CriteriaOwner = "BRTestQueue" AND
      CAN-FIND(FIRST ttCriteria WHERE
                  ttCriteria.BRTestCaseID = iiBRTestCaseID AND
                  ttCriteria.CriteriaTable = BRTestCriteria.CriteriaTable AND
                  ttCriteria.CriteriaField = BRTestCriteria.CriteriaField AND
                  ttCriteria.EventDateTo   = BRTestCriteria.EventDateTo AND
                  ttCriteria.CriteriaOwner = "BRTestCase")
   THEN RETURN "".
                     
   CREATE ttCriteria.
   BUFFER-COPY BRTestCriteria TO ttCriteria.
   ASSIGN 
      ttCriteria.BRTestCaseID = iiBRTestCaseID
      ttCriteria.CheckProc = lcCheckProc. 
         
   IF NOT fGetEventDates(ttCriteria.EventDateFrom,
                         ttCriteria.EventDateTo,
                         idaPeriodBeg,
                         idaPeriodEnd,
                         OUTPUT ttCriteria.BegDate,
                         OUTPUT ttCriteria.EndDate) THEN 
      RETURN "ERROR:Invalid period on criteria " + 
             STRING(BRTestCriteria.BRTestCriteriaID) + 
             ", case " + STRING(iiBRTestCaseID).

   ASSIGN 
      ttCriteria.PeriodBeg = fMake2DT(ttCriteria.BegDate,0)
      ttCriteria.PeriodEnd = fMake2DT(ttCriteria.EndDate,86399).
            
   /* get some periodical contract related data ready */ 
   IF ttCriteria.CriteriaTable = "DayCampaign" THEN DO:
      FOR EACH ServiceLimit NO-LOCK WHERE
               ServiceLimit.GroupCode MATCHES(ttCriteria.ValueIncluded):
         IF NOT CAN-FIND(FIRST ttServiceLimit WHERE 
            ttServiceLimit.SlSeq = ServiceLimit.SlSeq) THEN DO:
               CREATE ttServiceLimit.
               BUFFER-COPY ServiceLimit TO ttServiceLimit.
         END.
      END.
   END.   
   
   RETURN "". 

END PROCEDURE.

PROCEDURE pInitMobCDR:

   DEF INPUT PARAMETER iiBRTestCaseID AS INT  NO-UNDO.
   DEF INPUT PARAMETER icCheckProc    AS CHAR NO-UNDO.

   IF BRTestCriteria.CriteriaOwner = "BRTestQueue" AND
         CAN-FIND(FIRST ttCriteria WHERE
                   ttCriteria.BRTestCaseID = iiBRTestCaseID AND
                   ttCriteria.CriteriaTable = BRTestCriteria.CriteriaTable AND
                   ttCriteria.EventDateTo   = BRTestCriteria.EventDateTo AND
                   ttCriteria.CriteriaOwner = "BRTestCase")
   THEN RETURN "".
                                       
   FIND FIRST ttCriteria WHERE 
      ttCriteria.BRTestCaseID = iiBRTestCaseID AND
      ttCriteria.CriteriaTable = "MobCDR" NO-ERROR.
   IF NOT AVAILABLE ttCriteria THEN DO:
      CREATE ttCriteria.
      BUFFER-COPY BRTestCriteria TO ttCriteria.
      ASSIGN
         ttCriteria.BRTestCaseID = iiBRTestCaseID
         ttCriteria.CheckProc = icCheckProc.
   END. 

   CREATE ttField.
   ASSIGN 
      ttField.BRTestCaseID = ttCriteria.BRTestCaseID
      ttField.CriteriaTable = ttCriteria.CriteriaTable
      ttField.CriteriaField = BRTestCriteria.CriteriaField
      ttField.ROValueIncluded = BRTestCriteria.ROValueIncluded
      ttField.ValueIncluded = BRTestCriteria.ValueIncluded
      ttField.EventDateTo = BRTestCriteria.EventDateTo
      ttField.SingleCDR = (LOOKUP(ttField.CriteriaField,
                                  "Qty,BillDur,DataAmt,Amount") = 0).
                                 
   IF NOT fGetEventDates(BRTestCriteria.EventDateFrom,
                         BRTestCriteria.EventDateTo,
                         idaPeriodBeg,
                         idaPeriodEnd,
                         OUTPUT ttField.BegDate,
                         OUTPUT ttField.EndDate) THEN 
       RETURN "ERROR:Invalid period on criteria " + 
              STRING(BRTestCriteria.BRTestCriteriaID) + 
              ", case " + STRING(iiBRTestCaseID).
    
   IF ttCriteria.BegDate = ? THEN ASSIGN 
      ttCriteria.BegDate = ttField.BegDate
      ttCriteria.EndDate = ttField.EndDate.
   ELSE ASSIGN 
      ttCriteria.BegDate = MIN(ttCriteria.BegDate,ttField.BegDate)
      ttCriteria.EndDate = MAX(ttCriteria.EndDate,ttField.EndDate).

   ASSIGN 
      ttCriteria.PeriodBeg = fMake2DT(ttCriteria.BegDate,0)
      ttCriteria.PeriodEnd = fMake2DT(ttCriteria.EndDate,86399).
       
   RETURN "".
   
END PROCEDURE.

PROCEDURE pInitInvRowCounter:

   DEF INPUT PARAMETER iiBRTestCaseID AS INT  NO-UNDO.
   DEF INPUT PARAMETER icCheckProc    AS CHAR NO-UNDO.

   IF BRTestCriteria.CriteriaOwner = "BRTestQueue" AND
         CAN-FIND(FIRST ttCriteria WHERE
                   ttCriteria.BRTestCaseID = iiBRTestCaseID AND
                   ttCriteria.CriteriaTable = BRTestCriteria.CriteriaTable AND
                   ttCriteria.EventDateTo   = BRTestCriteria.EventDateTo AND
                   ttCriteria.CriteriaOwner = "BRTestCase")
   THEN RETURN "".
 
   FIND FIRST ttCriteria WHERE 
      ttCriteria.BRTestCaseID = iiBRTestCaseID AND
      ttCriteria.CriteriaTable = "InvRowCounter" NO-ERROR.
   IF NOT AVAILABLE ttCriteria THEN DO:
      CREATE ttCriteria.
      BUFFER-COPY BRTestCriteria TO ttCriteria.
      ASSIGN
         ttCriteria.BRTestCaseID = iiBRTestCaseID
         ttCriteria.CheckProc = icCheckProc.
   END. 

   CREATE ttField.
   ASSIGN 
      ttField.BRTestCaseID = ttCriteria.BRTestCaseID
      ttField.CriteriaTable = ttCriteria.CriteriaTable
      ttField.CriteriaField = BRTestCriteria.CriteriaField
      ttField.ROValueIncluded = BRTestCriteria.ROValueIncluded
      ttField.ValueIncluded = BRTestCriteria.ValueIncluded
      ttField.EventDateTo = BRTestCriteria.EventDateTo
      ttField.SingleCDR = (LOOKUP(ttField.CriteriaField,
                                  "Quantity,Duration,DataAmt,Amount") = 0).
                                 
   IF NOT fGetEventDates(BRTestCriteria.EventDateFrom,
                         BRTestCriteria.EventDateTo,
                         idaPeriodBeg,
                         idaPeriodEnd,
                         OUTPUT ttField.BegDate,
                         OUTPUT ttField.EndDate) THEN 
       RETURN "ERROR:Invalid period on criteria " + 
              STRING(BRTestCriteria.BRTestCriteriaID) + 
              ", case " + STRING(ttCriteria.BRTestCaseID).
    
   IF ttCriteria.BegDate = ? THEN ASSIGN 
      ttCriteria.BegDate = ttField.BegDate
      ttCriteria.EndDate = ttField.EndDate.
   ELSE ASSIGN 
      ttCriteria.BegDate = MIN(ttCriteria.BegDate,ttField.BegDate)
      ttCriteria.EndDate = MAX(ttCriteria.EndDate,ttField.EndDate).

   ASSIGN 
      ttCriteria.PeriodBeg = fMake2DT(ttCriteria.BegDate,0)
      ttCriteria.PeriodEnd = fMake2DT(ttCriteria.EndDate,86399).
 
   RETURN "".
   
END PROCEDURE.

PROCEDURE pCollect:

   DEF VAR llMatch     AS LOG  NO-UNDO.
   DEF VAR liLoop      AS INT  NO-UNDO. 
   DEF VAR llCreateInv AS LOG  NO-UNDO.
   
   GetSubscription:
   FOR EACH MsOwner NO-LOCK WHERE
            MsOwner.PayType = FALSE
   BREAK BY MsOwner.InvCust
         BY MsOwner.MsSeq
         BY MsOwner.TSEnd DESC:
   
      IF FIRST-OF(MsOwner.InvCust) THEN llCreateInv = FALSE.
      
      IF FIRST-OF(MsOwner.MsSeq) THEN DO:
      
         liCheck = liCheck + 1.
         IF NOT SESSION:BATCH AND liCheck MOD 100 = 0 THEN DO:
            PAUSE 0.
            DISP liCheck oiPicked WITH FRAME fQty.
         END.
 
         CheckCase:
         FOR EACH ttCase WHERE
                  ttCase.CaseDone = FALSE:
      
            FIND FIRST ttPickCust WHERE ttPickCust.InvCust = MsOwner.InvCust
               NO-ERROR.

            /* check if another subscription from this customer already 
               picked for this case 
            IF AVAILABLE ttPickCust THEN 
               DO liLoop = 1 TO NUM-ENTRIES(ttPickCust.CaseList): 
                  IF STRING(ttCase.BRTestCaseID) = 
                     ENTRY(2,ENTRY(liLoop,ttPickCust.CaseList),":")
                  THEN NEXT CheckCase. 
               END.
            */
         
            llMatch = TRUE.
         
            /* all given criteria must match */
            FOR EACH ttCriteria WHERE
                     ttCriteria.BRTestCaseID = ttCase.BRTestCaseID:

               /* subscription must have been valid during the 
                  criteria period */
               IF MsOwner.TSEnd < ttCriteria.PeriodBeg  OR 
                  MsOwner.TsBeg > ttCriteria.PeriodEnd THEN NEXT CheckCase. 
               
               RUN VALUE(ttCriteria.CheckProc) (OUTPUT llMatch) NO-ERROR.    

               IF RETURN-VALUE BEGINS "ERROR" THEN RETURN RETURN-VALUE.
               IF NOT llMatch THEN NEXT CheckCase.
            END.
      
            /* valid for this case */
            IF NOT AVAILABLE ttPickCust THEN DO:
               CREATE ttPickCust.
               ASSIGN 
                  ttPickCust.BRTestCaseID = ttCase.BRTestCaseID
                  ttPickCust.InvCust = MsOwner.InvCust
                  liCustQty = liCustQty + 1.
            END.
            ASSIGN 
               ttPickCust.CaseList = ttPickCust.CaseList + 
                               (IF ttPickCust.CaseList > "" THEN "," ELSE "") +
                               STRING(MsOwner.MsSeq) + ":" + 
                               STRING(ttCase.BRTestCaseID)
               oiPicked = oiPicked + 1
               ttCase.Picked = ttCase.Picked + 1
               llCreateInv = TRUE.

            IF ttCase.Picked >= ttCase.CaseQty THEN DO:
               ttCase.CaseDone = TRUE.
               IF NOT CAN-FIND(FIRST ttCase WHERE ttCase.CaseDone = FALSE) THEN
                  LEAVE GetSubscription.
            END.     
         END.
         
      END.
      
      /* create invoice immediately */
      IF LAST-OF(MsOwner.InvCust) AND llCreateInv THEN
         RUN pCreateInvoice(MsOwner.InvCust).
      
      IF iiUpdateInterval > 0 AND oiPicked MOD iiUpdateInterval = 0 
      THEN DO:
         IF NOT fUpdateFuncRunProgress(iiFRProcessID,oiPicked) THEN
            RETURN "ERROR:Stopped".
      END.   
      
   END.
   
   RETURN "".
   
END PROCEDURE.

PROCEDURE pCheckCLIType:

   DEF OUTPUT PARAMETER olMatch AS LOG  NO-UNDO.
   
   DEF VAR lcCLIType   AS CHAR NO-UNDO.
   
   DEF BUFFER bMsOwner FOR MsOwner.
  
   /* get clitype from desired period */
   IF MsOwner.TsEnd > ttCriteria.PeriodBeg AND 
      MsOwner.TsBeg < ttCriteria.PeriodEnd THEN lcCLIType = MsOwner.CLIType.
   ELSE FOR FIRST bMsOwner NO-LOCK WHERE
      bMsOwner.MsSeq = MsOwner.MsSeq AND
      bMsOwner.InvCust = MsOwner.InvCust AND
      bMsOwner.TsEnd > ttCriteria.PeriodBeg AND
      bMsOwner.TsBeg < ttCriteria.PeriodEnd:
         lcCLIType = bMsOwner.CLIType.
   END.
             
   olMatch = (lcCLIType MATCHES(ttCriteria.ValueIncluded)).

   /* reverse result */
   IF ttCriteria.ROValueIncluded = "NE" THEN olMatch = NOT olMatch.

   RETURN "".   

END PROCEDURE.

PROCEDURE pCheckOrder:

   DEF OUTPUT PARAMETER olMatch AS LOG  NO-UNDO.
   
   olMatch = FALSE.
   FOR EACH Order NO-LOCK WHERE
            Order.MsSeq = MsOwner.MsSeq AND
            Order.StatusCode = {&ORDER_STATUS_DELIVERED} AND
            Order.CrStamp >= ttCriteria.PeriodBeg AND
            Order.CrStamp <= ttCriteria.PeriodEnd:
      olMatch = (Order.OrderChannel MATCHES(ttCriteria.ValueIncluded)).
      IF olMatch THEN LEAVE.
   END.
   
   /* reverse result */
   IF ttCriteria.ROValueIncluded = "NE" THEN olMatch = NOT olMatch.

   RETURN "".   

END PROCEDURE.

PROCEDURE pCheckDayCampaign:
 
   DEF OUTPUT PARAMETER olMatch AS LOG  NO-UNDO.
 
   DEF VAR ldaNextPeriod AS DEC NO-UNDO.
   DEF VAR llRateLimit   AS LOG NO-UNDO.
 
   DEF BUFFER bLimit FOR MServiceLimit.
   DEF BUFFER bDCCLI FOR DCCLI.
   
   ASSIGN
      olMatch = FALSE
      llRateLimit = FALSE.
   
   /* special case dss; check if dss is active on customer */
   IF ttCriteria.ValueIncluded = {&DSS} THEN 
   CheckDSSLimit:
   FOR FIRST ttServiceLimit NO-LOCK WHERE
             ttServiceLimit.GroupCode = {&DSS},
       FIRST MServiceLimit NO-LOCK WHERE
             MServiceLimit.CustNum = MsOwner.InvCust AND
             MServiceLimit.DialType = ttServiceLimit.DialType AND
             MServiceLimit.SlSeq   = ttServiceLimit.SlSeq AND
             MServiceLimit.EndTS > ttCriteria.PeriodBeg AND
             MServiceLimit.FromTS < ttCriteria.PeriodEnd AND
             MServiceLimit.EndTS > MServiceLimit.FromTS:

      llRateLimit = TRUE.
         
      CASE ttCriteria.Setting:
      WHEN "Activated" THEN DO:
         IF MServiceLimit.FromTS >= ttCriteria.PeriodBeg AND
            MServiceLimit.FromTS <= ttCriteria.PeriodEnd THEN DO:
               olMatch = TRUE.
               LEAVE CheckDSSLimit.      
         END.
      END. 
         
      WHEN "Terminated" THEN DO:
         IF MServiceLimit.EndTS > ttCriteria.PeriodBeg AND
            MServiceLimit.EndTS <= ttCriteria.PeriodEnd THEN DO: 
               olMatch = TRUE.
               LEAVE CheckDSSLimit.      
         END.   
      END.

      OTHERWISE DO:
         olMatch = TRUE.
         LEAVE CheckDSSLimit.      
      END.
      END CASE.           
   END.          
   
   ELSE DO:
      ldaNextPeriod = fMake2DT(ttCriteria.EndDate + 1,0).
      
      CheckMLimit:
      FOR EACH ttServiceLimit NO-LOCK WHERE 
               ttServiceLimit.GroupCode MATCHES(ttCriteria.ValueIncluded),
          EACH MServiceLimit NO-LOCK WHERE
               MServiceLimit.MsSeq = MsOwner.MsSeq AND
               MServiceLimit.DialType = ttServiceLimit.DialType AND
               MServiceLimit.SlSeq = ttServiceLimit.SlSeq AND
               MServiceLimit.EndTS > ttCriteria.PeriodBeg AND
               MServiceLimit.FromTS < ttCriteria.PeriodEnd AND
               MServiceLimit.EndTS > MServiceLimit.FromTS:

         llRateLimit = TRUE.
         
         CASE ttCriteria.Setting:
         WHEN "Activated" THEN DO:
            IF MServiceLimit.FromTS >= ttCriteria.PeriodBeg AND
               MServiceLimit.FromTS <= ttCriteria.PeriodEnd THEN DO:
                  olMatch = TRUE.
                  LEAVE CheckMLimit.      
            END.
         END. 
         
         WHEN "Terminated" THEN DO:
            IF MServiceLimit.EndTS > ttCriteria.PeriodBeg AND
               MServiceLimit.EndTS <= ttCriteria.PeriodEnd THEN DO: 

               /* continues directly */
               IF MServiceLimit.EndTS = ttCriteria.PeriodEnd AND
                  CAN-FIND(FIRST bLimit WHERE
                                 bLimit.MsSeq = MServiceLimit.MsSeq AND
                                 bLimit.DialType = MServiceLimit.DialType AND
                                 bLimit.SlSeq = MServiceLimit.SlSeq AND
                                 bLimit.EndTS > ldaNextPeriod AND
                                 bLimit.FromTS = ldaNextPeriod) THEN NEXT.
               olMatch = TRUE.
               LEAVE CheckMLimit.      
            END.   
         END.

         WHEN "Active" THEN
            IF MServiceLimit.EndTS > fMakeTS() THEN DO:
               olMatch = TRUE.
               LEAVE CheckMLimit.
            END.

         OTHERWISE DO:
            olMatch = TRUE.
            LEAVE CheckMLimit.      
         END.
         END CASE.           
      END. /* CheckMLimit */
   END.
   
   IF NOT llRateLimit THEN DO:

      CheckDCCLI:
      FOR EACH DCCLI NO-LOCK WHERE
               DCCLI.MsSeq = MsOwner.MsSeq AND
               DCCLI.DCEvent MATCHES(ttCriteria.ValueIncluded) AND
               DCCLI.ValidTo > ttCriteria.BegDate AND
               DCCLI.ValidFrom <= ttCriteria.EndDate AND
               DCCLI.ValidTo > DCCLI.ValidFrom:

         CASE ttCriteria.Setting:
         WHEN "Activated" THEN DO:
            IF DCCLI.ValidFrom >= ttCriteria.BegDate AND
               DCCLI.ValidFrom <= ttCriteria.EndDate THEN DO:
                  olMatch = TRUE.       
                  LEAVE CheckDCCLI.
            END.
         END.

         WHEN "Terminated" THEN DO:
         
            IF DCCLI.ValidTo > ttCriteria.BegDate AND
               DCCLI.ValidTo <= ttCriteria.EndDate THEN DO:
   
               /* continues directly */
               IF DCCLI.ValidTo = ttCriteria.EndDate AND
                  CAN-FIND(FIRST bDCCLI WHERE
                                 bDCCLI.MsSeq = MsOwner.MsSeq AND
                                 bDCCLI.DCEvent = DCCLI.DCEvent AND
                                 bDCCLI.ValidTo > ttCriteria.EndDate + 1 AND
                                 bDCCLI.ValidFrom = ttCriteria.EndDate + 1)
               THEN NEXT. 
            
               olMatch = TRUE.       
               LEAVE CheckDCCLI.
            END.
         END.    

         WHEN "Active" THEN
            IF DCCLI.ValidTo > TODAY THEN DO:
               olMatch = TRUE.
               LEAVE CheckDCCLI.
            END.

         OTHERWISE DO:
            olMatch = TRUE.
            LEAVE CheckDCCLI.
         END.
         END CASE.
         
      END. /* CheckDCCLI */   
   END.

   /* reverse result */
   IF ttCriteria.ROValueIncluded = "NE" THEN olMatch = NOT olMatch.

   RETURN "".   
   
END PROCEDURE.

PROCEDURE pCheckMobSub:
 
   DEF OUTPUT PARAMETER olMatch AS LOG  NO-UNDO.
   
   DEF BUFFER bMsOwner FOR MsOwner.
   DEF BUFFER bPrevOwner FOR MsOwner.
   
   olMatch = FALSE.
   
   CASE ttCriteria.ValueIncluded:
   WHEN "Activated" THEN 
      FOR EACH bMsOwner NO-LOCK WHERE
               bMsOwner.MsSeq = MsOwner.MsSeq AND
               bMsOwner.InvCust = MsOwner.InvCust AND
               bMsOwner.TSEnd > ttCriteria.PeriodBeg AND
               bMsOwner.TSBeg >= ttCriteria.PeriodBeg AND
               bMsOwner.TSBeg <= ttCriteria.PeriodEnd:
         IF CAN-FIND(FIRST bPrevOwner WHERE
                           bPrevOwner.MsSeq = bMsOwner.MsSeq AND
                           bPrevOwner.InvCust = bMsOwner.InvCust AND
                           bPrevOwner.TSBeg < bMsOwner.TSBeg)
         THEN NEXT. 
         
         olMatch = TRUE.       
         LEAVE.
      END.

   WHEN "Terminated" THEN 
      FOR FIRST TermMobSub NO-LOCK WHERE
                TermMobSub.MsSeq = MsOwner.MsSeq AND
                TermMobSub.InvCust = MsOwner.InvCust,
          FIRST bMsOwner NO-LOCK WHERE
                bMsOwner.MsSeq = MsOwner.MsSeq:
                
         IF bMsOwner.TSEnd >= ttCriteria.PeriodBeg AND
            bMsOwner.TSEnd <= ttCriteria.PeriodEnd THEN 
               olMatch = TRUE.       
      END.
   END.

   /* reverse result */
   IF ttCriteria.ROValueIncluded = "NE" THEN olMatch = NOT olMatch.

   RETURN "".   
                
END PROCEDURE.

PROCEDURE pCheckLimit:
 
   DEF OUTPUT PARAMETER olMatch AS LOG  NO-UNDO.
   
   olMatch = FALSE.
   
   CheckLimit:
   FOR EACH Limit NO-LOCK USE-INDEX MsSeq WHERE
            Limit.MsSeq     = MsOwner.MsSeq  AND
            Limit.LimitType = 3              AND
            Limit.TMRuleSeq = 0              AND
            Limit.ToDate   > ttCriteria.BegDate  AND
            Limit.FromDate <= ttCriteria.EndDate AND
            Limit.ToDate > Limit.FromDate    AND
            Limit.LimitID = 0                AND
            Limit.CustNum = MsOwner.InvCust  AND
            STRING(Limit.LimitAmt) MATCHES(ttCriteria.ValueIncluded):

      CASE ttCriteria.Setting:
      WHEN "Activated" THEN DO:
         IF Limit.FromDate >= ttCriteria.BegDate AND
            Limit.FromDate <= ttCriteria.EndDate THEN DO:
               olMatch = TRUE.       
               LEAVE CheckLimit.
         END.
      END.

      WHEN "Terminated" THEN DO:
         IF Limit.ToDate > ttCriteria.BegDate AND
            Limit.ToDate <= ttCriteria.EndDate THEN DO:
               olMatch = TRUE.       
               LEAVE CheckLimit.
         END.
      END.

      OTHERWISE DO:
         IF Limit.ToDate > ttCriteria.EndDate THEN DO:
            olMatch = TRUE.       
            LEAVE CheckLimit.
         END.
      END.

      END CASE. 
   END.   

   /* reverse result */
   IF ttCriteria.ROValueIncluded = "NE" THEN olMatch = NOT olMatch.

   RETURN "".   
       
END PROCEDURE.

PROCEDURE pCheckTaxZone:
 
   DEF OUTPUT PARAMETER olMatch AS LOG  NO-UNDO.
   
   olMatch = FALSE.
   
   FOR FIRST Customer NO-LOCK WHERE
             Customer.CustNum = MsOwner.InvCust:
      IF fRegionTaxZone(Customer.Region) = ttCriteria.ValueIncluded THEN
         olMatch = TRUE.
   END.

   /* reverse result */
   IF ttCriteria.ROValueIncluded = "NE" THEN olMatch = NOT olMatch.

   RETURN "".   
    
END PROCEDURE.

PROCEDURE pCheckMsRequest:
 
   DEF OUTPUT PARAMETER olMatch AS LOG  NO-UNDO.
   
   DEF VAR liReqType AS INT  NO-UNDO.
   DEF VAR liReqStat AS INT  NO-UNDO.
   
   olMatch = FALSE.
   
   liReqType = INT(ttCriteria.ValueIncluded) NO-ERROR.
   IF ERROR-STATUS:ERROR THEN RETURN 
      "ERROR:Invalid request type definition, case " + 
      STRING(ttCriteria.BRTestCaseID).
   
   IF ttCriteria.Setting BEGINS "Status=" THEN DO:
      liReqStat = INT(ENTRY(2,ttCriteria.Setting,"=")) NO-ERROR.
      IF ERROR-STATUS:ERROR THEN 
         RETURN "ERROR:Invalid request status definition, case " + 
                STRING(ttCriteria.BRTestCaseID).
   END.
   ELSE liReqStat = 2.

   FOR FIRST MsRequest NO-LOCK WHERE 
             MsRequest.MsSeq = MsOwner.MsSeq AND
             MsRequest.ReqType = liReqType AND
             MsRequest.ReqStat = liReqStat AND
             MsRequest.ActStamp >= ttCriteria.PeriodBeg AND
             MsRequest.ActStamp <= ttCriteria.PeriodEnd:
      olMatch = TRUE.
   END.

   /* reverse result */
   IF ttCriteria.ROValueIncluded = "NE" THEN olMatch = NOT olMatch.

   RETURN "".   
             
END PROCEDURE.

PROCEDURE pCheckMobCDR:
 
   DEF OUTPUT PARAMETER olMatch AS LOG  NO-UNDO.
   
   DEF VAR liQty      AS INT  NO-UNDO.
   DEF VAR liDuration AS INT  NO-UNDO.
   DEF VAR ldDataAmt  AS DEC  NO-UNDO.
   DEF VAR ldAmount   AS DEC  NO-UNDO.
   DEF VAR ldCriteria AS DEC  NO-UNDO.
   DEF VAR ldCDRSum   AS DEC  NO-UNDO.
   DEF VAR llSumMatch AS LOG  NO-UNDO.
   DEF VAR llSumCheck AS LOG  NO-UNDO.
   DEF VAR llNEMatch  AS LOG  NO-UNDO.
   
   ASSIGN
      olMatch = FALSE
      llNEMatch = TRUE.
   
   /* billable cdrs are always in the latest db so get them directly */
   GetCDR:
   FOR EACH InvSeq NO-LOCK WHERE
            InvSeq.MsSeq = MsOwner.MsSeq  AND
            InvSeq.CustNum = MsOwner.InvCust AND
            InvSeq.Billed = FALSE AND
            InvSeq.ToDate >= ttCriteria.BegDate AND
            InvSeq.FromDate <= ttCriteria.EndDate,
       EACH MobCDR NO-LOCK WHERE
            MobCDR.InvCust = InvSeq.CustNum AND
            MobCDR.InvSeq  = InvSeq.InvSeq AND 
            MobCDR.DateSt >= ttCriteria.BegDate AND
            MobCDR.DateSt <= ttCriteria.EndDate:
            
      FOR EACH ttField WHERE
               ttField.BRTestCaseID = ttCriteria.BRTestCaseID AND
               ttField.CriteriaTable = ttCriteria.CriteriaTable AND
               ttField.SingleCDR = TRUE:
         lhField = lhCDR:BUFFER-FIELD(ttField.CriteriaField) NO-ERROR.
         IF ERROR-STATUS:ERROR OR NOT VALID-HANDLE(lhField) THEN 
            RETURN "ERROR:Invalid CDR field definition " + 
                   ttField.CriteriaField + ", case " + 
                   STRING(ttCriteria.BRTestCaseID).
                   
         IF ttField.ROValueIncluded = "NE" THEN DO:
            /* if just one item conflicts with the NE criteria it is enough
               to cause the subscription to be skipped */
            IF lhField:BUFFER-VALUE MATCHES ttField.ValueIncluded THEN DO:
               llNEMatch = FALSE.
               LEAVE GetCDR.
            END.   
         END.     
         ELSE IF NOT lhField:BUFFER-VALUE MATCHES ttField.ValueIncluded THEN 
            NEXT GetCDR.   
      END.
            
      ASSIGN 
         olMatch = TRUE
         liQty      = liQty + 1
         liDuration = liDuration + MobCDR.BillDur
         ldDataAmt  = ldDataAmt + MobCDR.DataIn + MobCDR.DataOut
         ldAmount   = ldAmount + MobCDR.Amount.
   END.         

   IF NOT llNEMatch THEN DO:
      olMatch = FALSE.
      RETURN "".
   END.
              
   ASSIGN 
      llSumMatch = TRUE
      llSumCheck = FALSE.
   /* check sum level criteria */
   FOR EACH ttField WHERE
            ttField.BRTestCaseID = ttCriteria.BRTestCaseID AND
            ttField.CriteriaTable = ttCriteria.CriteriaTable AND
            ttField.SingleCDR = FALSE:
      ldCriteria = DEC(ttField.ValueIncluded) NO-ERROR.
      IF ERROR-STATUS:ERROR THEN 
         RETURN "ERROR:Invalid amount in " + ttField.CriteriaField + 
                ", case " + STRING(ttCriteria.BRTestCaseID).
         
      llSumCheck = TRUE.
      
      CASE ttField.CriteriaField:
      WHEN "Qty"     THEN ldCDRSum = liQty.
      WHEN "BillDur" THEN ldCDRSum = liDuration.
      WHEN "DataAmt" THEN ldCDRSum = ldDataAmt / (1024 * 1024).
      WHEN "Amount"  THEN ldCDRSum = ldAmount.
      END CASE.
      
      CASE ttField.ROValueIncluded:
      WHEN "EQ" THEN IF ldCriteria NE ldCDRSum THEN llSumMatch = FALSE.
      WHEN "NE" THEN IF ldCriteria =  ldCDRSum THEN llSumMatch = FALSE.
      WHEN "LT" THEN IF ldCriteria <= ldCDRSum THEN llSumMatch = FALSE.
      WHEN "LE" THEN IF ldCriteria <  ldCDRSum THEN llSumMatch = FALSE.
      WHEN "GT" THEN IF ldCriteria >= ldCDRSum THEN llSumMatch = FALSE.
      WHEN "GE" THEN IF ldCriteria >  ldCDRSum THEN llSumMatch = FALSE.
      END CASE.
      
      IF NOT llSumMatch THEN LEAVE.
   END.

   /* if no cdrs were found then sum level result is valid (purpose is to 
      find cases e.g. with no traffic). */
   IF liQty = 0 THEN DO:
      IF llSumCheck THEN olMatch = llSumMatch. 
   END.
   ELSE DO:
      IF olMatch AND llSumCheck THEN olMatch = llSumMatch.
   END.   
   
   RETURN "".   
   
END PROCEDURE.

PROCEDURE pCheckInvRowCounter:
 
   DEF OUTPUT PARAMETER olMatch AS LOG  NO-UNDO.
   
   DEF VAR liQty      AS INT  NO-UNDO.
   DEF VAR liDuration AS INT  NO-UNDO.
   DEF VAR ldDataAmt  AS DEC  NO-UNDO.
   DEF VAR ldAmount   AS DEC  NO-UNDO.
   DEF VAR ldCriteria AS DEC  NO-UNDO.
   DEF VAR ldCDRSum   AS DEC  NO-UNDO.
   DEF VAR llSumMatch AS LOG  NO-UNDO.
   DEF VAR llSumCheck AS LOG  NO-UNDO.
   DEF VAR llNEMatch  AS LOG  NO-UNDO.
   
   ASSIGN 
      olMatch = FALSE
      llNEMatch = TRUE.
      
   GetCounter:
   FOR EACH InvSeq NO-LOCK WHERE
            InvSeq.MsSeq = MsOwner.MsSeq  AND
            InvSeq.CustNum = MsOwner.InvCust AND
            InvSeq.Billed = FALSE AND
            InvSeq.ToDate >= ttCriteria.BegDate AND
            InvSeq.FromDate <= ttCriteria.EndDate,
       EACH InvRowCounter NO-LOCK WHERE
            InvRowCounter.InvCust = InvSeq.CustNum AND
            InvRowCounter.InvSeq = InvSeq.InvSeq:
            
      FOR EACH ttField WHERE
               ttField.BRTestCaseID = ttCriteria.BRTestCaseID AND
               ttField.CriteriaTable = ttCriteria.CriteriaTable AND
               ttField.SingleCDR = TRUE:
         lhField = lhCounter:BUFFER-FIELD(ttField.CriteriaField) NO-ERROR.
         IF ERROR-STATUS:ERROR OR NOT VALID-HANDLE(lhField) THEN 
            RETURN "ERROR:Invalid counter field definition " + 
                   ttField.CriteriaField + ", case " + 
                   STRING(ttCriteria.BRTestCaseID).
                   
         IF ttField.ROValueIncluded = "NE" THEN DO: 
            /* if just one item conflicts with the NE criteria it is enough
               to cause the subscription to be skipped */
            IF lhField:BUFFER-VALUE MATCHES ttField.ValueIncluded THEN DO:
               llNEMatch = FALSE.
               LEAVE GetCounter.
            END.   
         END.      
         ELSE IF NOT lhField:BUFFER-VALUE MATCHES ttField.ValueIncluded THEN 
            NEXT GetCounter.   
      END.
            
      ASSIGN 
         olMatch = TRUE
         liQty      = liQty + InvRowCounter.Quantity
         liDuration = liDuration + InvRowCounter.Duration
         ldDataAmt  = ldDataAmt + InvRowCounter.DataAmt
         ldAmount   = ldAmount + InvRowCounter.Amount.
   END.         
             
   IF NOT llNEMatch THEN DO:
      olMatch = FALSE.
      RETURN "".
   END.
   
   ASSIGN 
      llSumMatch = TRUE
      llSumCheck = FALSE.
   /* check sum level criteria */
   FOR EACH ttField WHERE
            ttField.BRTestCaseID = ttCriteria.BRTestCaseID AND
            ttField.CriteriaTable = ttCriteria.CriteriaTable AND
            ttField.SingleCDR = FALSE:
      ldCriteria = DEC(ttField.ValueIncluded) NO-ERROR.
      IF ERROR-STATUS:ERROR THEN 
         RETURN "ERROR:Invalid amount in " + ttField.CriteriaField + 
                ", case " + STRING(ttCriteria.BRTestCaseID).
         
      llSumCheck = TRUE.
      
      CASE ttField.CriteriaField:
      WHEN "Quantity" THEN ldCDRSum = liQty.
      WHEN "Duration" THEN ldCDRSum = liDuration.
      WHEN "DataAmt"  THEN ldCDRSum = ldDataAmt / (1024 * 1024).
      WHEN "Amount"   THEN ldCDRSum = ldAmount.
      END CASE.
      
      CASE ttField.ROValueIncluded:
      WHEN "EQ" THEN IF ldCriteria NE ldCDRSum THEN llSumMatch = FALSE.
      WHEN "NE" THEN IF ldCriteria =  ldCDRSum THEN llSumMatch = FALSE.
      WHEN "LT" THEN IF ldCriteria <= ldCDRSum THEN llSumMatch = FALSE.
      WHEN "LE" THEN IF ldCriteria <  ldCDRSum THEN llSumMatch = FALSE.
      WHEN "GT" THEN IF ldCriteria >= ldCDRSum THEN llSumMatch = FALSE.
      WHEN "GE" THEN IF ldCriteria >  ldCDRSum THEN llSumMatch = FALSE.
      END CASE.
      
      IF NOT llSumMatch THEN LEAVE.
   END.

   /* if no counters were found then sum level result is valid (purpose is to 
      find cases e.g. with no traffic). */
   IF liQty = 0 THEN DO:
      IF llSumCheck THEN olMatch = llSumMatch. 
   END.
   ELSE DO:
      IF olMatch AND llSumCheck THEN olMatch = llSumMatch.
   END.   
   
   RETURN "".   
   
END PROCEDURE.

PROCEDURE pCheckFixedFee:
 
   DEF OUTPUT PARAMETER olMatch AS LOG  NO-UNDO.

   DEF VAR liFrom AS INT  NO-UNDO.
   DEF VAR liTo   AS INT  NO-UNDO.
   
   ASSIGN
      olMatch = FALSE
      liFrom  = TRUNCATE(ttCriteria.PeriodBeg / 100,0)
      liTo    = TRUNCATE(ttCriteria.PeriodEnd / 100,0).

   IF ttCriteria.CriteriaField = "BillCode" THEN
      FOR EACH FixedFee NO-LOCK WHERE
               FixedFee.Brand = gcBrand AND
               FixedFee.HostTable = "MobSub" AND
               FixedFee.KeyValue = STRING(MsOwner.MsSeq) AND
               FixedFee.InUse = TRUE AND
               FixedFee.BillCode MATCHES(ttCriteria.ValueIncluded),
         FIRST FFItem OF FixedFee NO-LOCK WHERE
               FFItem.BillPeriod >= liFrom AND
               FFItem.BillPeriod <= liTo AND
               FFItem.Billed = FALSE:
      
         CASE ttCriteria.Setting:
            WHEN "TF=FinancedByUNOEBank" THEN IF
               NOT LOOKUP(FixedFee.FinancedResult,{&TF_STATUSES_BANK}) > 0 OR
               FixedFee.TFBank <> "0049" THEN NEXT.
            WHEN "TF=FinancedBySabaDellBank" THEN IF
               NOT LOOKUP(FixedFee.FinancedResult,{&TF_STATUSES_BANK}) > 0 OR
               FixedFee.TFBank <> "0081" THEN NEXT.
            WHEN "TF=FinancedByYoigo" THEN IF
               NOT (FixedFee.FinancedResult EQ "" OR
               FixedFee.FinancedResult BEGINS "Y") THEN NEXT.
            WHEN "TF=NotSentToBank" THEN IF
               NOT FixedFee.FinancedResult EQ {&TF_STATUS_WAITING_SENDING} THEN NEXT.
            WHEN "TF=SentToBank" THEN IF
               NOT FixedFee.FinancedResult EQ {&TF_STATUS_SENT_TO_BANK} THEN NEXT.
         END.

         olMatch = TRUE.
         LEAVE.
      END.
   ELSE IF ttCriteria.CriteriaField = "CalcObj" THEN
      FOR EACH FixedFee NO-LOCK WHERE
               FixedFee.Brand = gcBrand AND
               FixedFee.HostTable = "MobSub" AND
               FixedFee.KeyValue = STRING(MsOwner.MsSeq) AND
               FixedFee.InUse = TRUE AND
               FixedFee.CalcObj MATCHES(ttCriteria.ValueIncluded),
         FIRST FFItem OF FixedFee NO-LOCK WHERE
               FFItem.BillPeriod >= liFrom AND
               FFItem.BillPeriod <= liTo AND
               FFItem.Billed = FALSE:
      
         CASE ttCriteria.Setting:
            WHEN "TF=FinancedByUNOEBank" THEN IF
               NOT LOOKUP(FixedFee.FinancedResult,{&TF_STATUSES_BANK}) > 0 OR
               FixedFee.TFBank <> "0049" THEN NEXT.
            WHEN "TF=FinancedBySabaDellBank" THEN IF
               NOT LOOKUP(FixedFee.FinancedResult,{&TF_STATUSES_BANK}) > 0 OR
               FixedFee.TFBank <> "0081" THEN NEXT.
            WHEN "TF=FinancedByYoigo" THEN IF
               NOT (FixedFee.FinancedResult EQ "" OR
               FixedFee.FinancedResult BEGINS "Y") THEN NEXT.
            WHEN "TF=NotSentToBank" THEN IF
               NOT FixedFee.FinancedResult EQ {&TF_STATUS_WAITING_SENDING} THEN NEXT.
            WHEN "TF=SentToBank" THEN IF
               NOT FixedFee.FinancedResult EQ {&TF_STATUS_SENT_TO_BANK} THEN NEXT.
         END.

         olMatch = TRUE.
         LEAVE.
      END.
  
   /* reverse result */
   IF ttCriteria.ROValueIncluded = "NE" THEN olMatch = NOT olMatch.

   RETURN "".   
 
END PROCEDURE. 

PROCEDURE pCheckSingleFee:
 
   DEF OUTPUT PARAMETER olMatch AS LOG  NO-UNDO.

   DEF VAR liFrom AS INT  NO-UNDO.
   DEF VAR liTo   AS INT  NO-UNDO.
   
   ASSIGN
      olMatch = FALSE
      liFrom  = TRUNCATE(ttCriteria.PeriodBeg / 100,0)
      liTo    = TRUNCATE(ttCriteria.PeriodEnd / 100,0).
   
   FOR EACH SingleFee NO-LOCK WHERE
            SingleFee.Brand = gcBrand AND
            SingleFee.HostTable = "MobSub" AND
            SingleFee.KeyValue = STRING(MsOwner.MsSeq) AND
            SingleFee.Active = TRUE AND
            SingleFee.BillCode MATCHES(ttCriteria.ValueIncluded) AND
            SingleFee.BillPeriod >= liFrom AND
            SingleFee.BillPeriod <= liTo AND
            SingleFee.Billed = FALSE:
      olMatch = TRUE.
      LEAVE.
   END.
            
   /* reverse result */
   IF ttCriteria.ROValueIncluded = "NE" THEN olMatch = NOT olMatch.

   RETURN "".   
 
END PROCEDURE. 

PROCEDURE pCheckFATime:
 
   DEF OUTPUT PARAMETER olMatch AS LOG  NO-UNDO.

   DEF VAR liFrom AS INT  NO-UNDO.
   DEF VAR liTo   AS INT  NO-UNDO.
   
   ASSIGN
      olMatch = FALSE
      liFrom  = TRUNCATE(ttCriteria.PeriodBeg / 100,0)
      liTo    = TRUNCATE(ttCriteria.PeriodEnd / 100,0).
   
   FOR EACH FATime NO-LOCK USE-INDEX MobSub WHERE
            FATime.Brand = gcBrand AND
            FATime.MsSeq = MsOwner.MsSeq AND
            FATime.InvNum = 0 AND
            FATime.FTGrp MATCHES(ttCriteria.ValueIncluded):
            
      IF FATime.Period > liTo OR
         (NOT Fatime.Transfer AND FATime.Period < liFrom)
      THEN NEXT.
               
      IF FATime.LastPeriod > 0 AND FATime.LastPeriod < liFrom 
      THEN NEXT. 

      IF FATime.TransPeriod > 0 THEN DO:
         IF FATime.TransPeriod <= liOldEventPeriod THEN NEXT.
      END.
      ELSE IF FATime.Period <= liOldEventPeriod THEN NEXT.

      olMatch = TRUE.
      LEAVE.
   END.
            
   /* reverse result */
   IF ttCriteria.ROValueIncluded = "NE" THEN olMatch = NOT olMatch.

   RETURN "".   
 
END PROCEDURE. 

PROCEDURE pCheckDPMember:
 
   DEF OUTPUT PARAMETER olMatch AS LOG  NO-UNDO.

   olMatch = FALSE.
   
   FOR EACH DPMember NO-LOCK WHERE
            DPMember.HostTable = "MobSub" AND
            DPMember.KeyValue = STRING(MsOwner.MsSeq) AND
            DPMember.ValidFrom <= ttCriteria.EndDate AND
            DPMember.ValidTo   >= ttCriteria.BegDate,
      FIRST DiscountPlan OF DPMember NO-LOCK WHERE
            DiscountPlan.DPRuleID MATCHES(ttCriteria.ValueIncluded):
            
      olMatch = TRUE.
      LEAVE.
   END.
            
   /* reverse result */
   IF ttCriteria.ROValueIncluded = "NE" THEN olMatch = NOT olMatch.

   RETURN "".   
 
END PROCEDURE. 

PROCEDURE pCreateInvoice:

   DEF INPUT PARAMETER iiInvCust AS INT  NO-UNDO.
   
   DEF VAR liCnt       AS INT  NO-UNDO.
   DEF VAR liCustQty   AS INT  NO-UNDO.

   DEF BUFFER bPickCust FOR ttPickCust.
   
   
   FOR FIRST Customer NO-LOCK WHERE
             Customer.CustNum = iiInvCust,
       FIRST bPickCust WHERE 
             bPickCust.InvCust = iiInvCust,
      FIRST InvGroup OF Customer NO-LOCK:

      EMPTY TEMP-TABLE ttInvCust.

      RUN bundle_first_month_fee.p(idaPeriodBeg,
                                   idaPeriodEnd,
                                   Customer.CustNum,
                                   0,
                                   0,
                                   "",
                                   OUTPUT liCnt).
 
      IF RETURN-VALUE BEGINS "ERROR" THEN DO:
         bPickCust.ErrorMsg = bPickCust.ErrorMsg +
             (IF bPickCust.ErrorMsg > "" THEN ", " ELSE "") +
             "Calculation for bundle first month fee failed".
         NEXT.
      END.

      /* If customer has DSS active then calculate bundle fee */
      RUN dss_bundle_first_month_fee.p(idaPeriodBeg,
                                       idaPeriodEnd,
                                       Customer.CustNum,
                                       0,
                                       0,
                                       "",
                                       OUTPUT liCnt).
 
      IF RETURN-VALUE BEGINS "ERROR" THEN DO:
         bPickCust.ErrorMsg = bPickCust.ErrorMsg +
             (IF bPickCust.ErrorMsg > "" THEN ", " ELSE "") +
             "Calculation for DSS first month fee failed".
         NEXT.
      END.
    
      CREATE ttInvCust.
      ASSIGN 
         ttInvCust.CustNr  = Customer.CustNum
         ttInvCust.MinInv  = InvGroup.MinInvAmt
         ttInvCust.CallQty = 0
         ttInvCust.LowVal  = FALSE
         ttPickCust.InvCreated = TRUE
         liCustQty = liCustQty + 1.
   END.
   
   IF CAN-FIND(FIRST ttInvCust) THEN 
      RUN pCreateTestInv in lhHandle("",   /* msseq list */
                                     idaInvDate,
                                     idaDueDate,
                                     idaPeriodBeg,
                                     idaPeriodEnd,
                                     iiFeePeriod,
                                     0,
                                     FALSE,
                                     FALSE,
                                     liCustQty,
                                     lcBillRun).
   
END PROCEDURE.

PROCEDURE pSaveResults:

   DEF VAR liCustCnt AS INT  NO-UNDO. 
   DEF VAR liRunQty  AS INT  NO-UNDO.
   DEF VAR liBatch   AS INT  NO-UNDO INIT 1.
   
   FOR EACH ttPickCust TRANS:

      CREATE FuncRunResult.
      ASSIGN 
         FuncRunResult.FRProcessID = iiFRProcessID
         FuncRunResult.FRExecID    = iiFRExecID
         FuncRunResult.FRResultSeq = liBatch
         FuncRunResult.IntParam    = ttPickCust.InvCust
         liCustCnt                 = liCustCnt + 1
         FuncRunResult.ResultOrder = liCustCnt
         FuncRunResult.CharParam   = ttPickCust.CaseList + 
            (IF ttPickCust.ErrorMsg > "" 
             THEN ",ERROR:" + ttPickCust.ErrorMsg
             ELSE "").
   END.
   
END PROCEDURE.


