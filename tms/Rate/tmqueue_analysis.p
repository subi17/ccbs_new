/* ----------------------------------------------------------------------------
  MODULE .......: tmqueue_analysis.p
  FUNCTION .....: Analyse rows in TMQueue 
  APPLICATION ..: TMS
  AUTHOR .......: aam 
  CREATED ......: 29.04.08
  Changed . ....: 
  Version ......: Yoigo
  --------------------------------------------------------------------------- */
&GLOBAL-DEFINE PREMIUM_BARRING_LIMIT 40 

{commpaa.i}
ASSIGN
   gcBrand = "1"
   katun   = "Cron".
   
{timestamp.i}
{cparam2.i}
{heartbeat.i}
{barrfunc.i}
{fmakemsreq.i}
{fsmsreq.i}
{tmsconst.i}
{fdestcountry.i}
{fbundle.i}
{tmqueue_analysis.i}
{istc.i}

DEF VAR lhSource      AS HANDLE NO-UNDO.
DEF VAR lhField       AS HANDLE NO-UNDO.
DEF VAR liField       AS INT    NO-UNDO. 
DEF VAR ldQty         AS DEC    NO-UNDO.
DEF VAR ldDone        AS DEC    NO-UNDO.
DEF VAR ldCounter     AS DEC    NO-UNDO.
DEF VAR liActions     AS INT    NO-UNDO.
DEF VAR llMatch       AS LOG    NO-UNDO.
DEF VAR liLoop        AS INT    NO-UNDO.
DEF VAR liInterval    AS INT    NO-UNDO.
DEF VAR liTransQty    AS INT    NO-UNDO.
DEF VAR lcCurrent     AS CHAR   NO-UNDO.
DEF VAR lcStarted     AS CHAR   NO-UNDO.
DEF VAR lcLogFile     AS CHAR   NO-UNDO.
DEF VAR ldCurrent     AS DEC    NO-UNDO.
DEF VAR liDays        AS INT    NO-UNDO.
DEF VAR liSeconds     AS INT    NO-UNDO.
DEF VAR ldValue       AS DEC    NO-UNDO.
DEF VAR ldtUpdated    AS DATE   NO-UNDO.
DEF VAR ldtLatest     AS DATE   NO-UNDO.
DEF VAR liLimitID     AS INT    NO-UNDO.
DEF VAR ldTempUpdated AS DEC    NO-UNDO.
DEF VAR lcTempUpdated AS CHAR   NO-UNDO.
DEF VAR llMonitor     AS LOG    NO-UNDO.
DEF VAR lcMonitor     AS CHAR   NO-UNDO.
DEF VAR llBarringDone AS LOG    NO-UNDO.
DEF VAR liTicketType  AS INT  NO-UNDO. 
DEF VAR lcCountry     AS CHAR NO-UNDO. 
DEF VAR lcCounterItem AS CHAR NO-UNDO. 
DEF VAR lcCounterItemValue AS CHAR NO-UNDO. 
DEF VAR liDSSUpsellLimit AS INT NO-UNDO. 
DEF VAR lcUpsells     AS CHAR NO-UNDO. 
DEF VAR lcUpsellsNew AS CHAR NO-UNDO. 
DEF VAR liUpSellCount AS INT  NO-UNDO. 
DEF VAR lcInvSeqList  AS CHAR NO-UNDO.
DEF VAR liInvSeqCnt   AS INT  NO-UNDO.
DEF VAR liInvSeq      AS INT  NO-UNDO.
DEF VAR ldPrevQty     AS DEC  NO-UNDO.
DEF VAR liTotalTrafficFraudSeq AS INT NO-UNDO. 
DEF VAR liPremiumFraudPostSeq AS INT NO-UNDO.
DEF VAR liPremiumFraudPreSeq AS INT NO-UNDO.
DEF VAR ldeAmount AS DEC NO-UNDO. 
DEF VAR lcCONTFVoice100 AS CHAR NO-UNDO.
DEF VAR ldeTotalLimit   AS DEC NO-UNDO.
DEF VAR ldeMonthBegin   AS DEC NO-UNDO.
DEF VAR ldeMonthEnd     AS DEC NO-UNDO.

DEF TEMP-TABLE ttRule NO-UNDO
   LIKE TMRule
   FIELD UseTopup  AS LOG
   FIELD MaxLimits AS INT.

DEF TEMP-TABLE ttItems NO-UNDO
   FIELD TMRuleSeq  AS INT
   FIELD Items      AS CHAR
   INDEX TMRuleSeq TMRuleSeq.

DEF TEMP-TABLE ttLimits NO-UNDO
   LIKE TMRLimit
   INDEX TMRuleSeq TMRuleSeq LimitID.

DEF TEMP-TABLE ttPeriod NO-UNDO
   FIELD TMRuleSeq   AS INT
   FIELD PeriodBegin AS DATE
   FIELD PeriodEnd   AS DATE
   INDEX PeriodEnd TMRuleSeq PeriodEnd.
   
DEF BUFFER bQueue FOR TMQueue.

DEF STREAM sLog.
DEF STREAM sDel.

FORM
   lcStarted     COLON 20 FORMAT "X(20)"         LABEL "Started" 
   lcTempUpdated COLON 20 FORMAT "X(20)"         LABEL "Configuration"
   lcCurrent     COLON 20 FORMAT "X(20)"         LABEL "Latest Loop" 
      SKIP(1)
   ldQty         COLON 20 FORMAT ">>>>>>>>>>>9"  LABEL "Events Read"
   ldtLatest     COLON 20 FORMAT "99-99-99"      LABEL "Latest Event Date"  
      SKIP(1)
   ldDone        COLON 20 FORMAT ">>>>>>>>>>>9"  LABEL "Events Handled"
   ldCounter     COLON 20 FORMAT ">>>>>>>>>>>9"  LABEL "Events To Counters"
   liActions     COLON 20 FORMAT ">>>>>>>>>>>9"  LABEL "Limit Actions"
WITH CENTERED ROW 5 SIDE-LABELS 
     TITLE " TMQUEUE FOR COUNTERS " FRAME fQty.

FUNCTION fErrorLog RETURNS LOGIC
  (iiCustNum AS INT,
   icCLI     AS CHAR,
   icError   AS CHAR):
 
   /* save to db for reporting */
   CREATE ErrorLog.
   ASSIGN ErrorLog.Brand     = gcBrand
          ErrorLog.ActionID  = "TMQUEUE"
          ErrorLog.TableName = "Customer"
          ErrorLog.KeyValue  = STRING(iiCustnum)
          ErrorLog.ErrorChar = icCLI
          ErrorLog.ErrorMsg  = icError
          ErrorLog.UserCode  = katun.
          ErrorLog.ActionTS  = fMakeTS().
    
END FUNCTION.

FUNCTION fUpsellBundleLimitNew RETURN LOGICAL
   ( INPUT iiTMRuleSeq AS INT):

   DEF VAR llIsDataBundleLimit AS LOG NO-UNDO.
   DEF VAR ldeTotalDataLimit   AS DEC NO-UNDO.

   llIsDataBundleLimit = (LOOKUP(STRING(iiTMRuleSeq),lcUpsellsNew) > 0).

   IF llIsDataBundleLimit AND TMCounter.LimitAmt = 0 THEN DO:
      liUpSellCount = fUpsellBundleCountNew(TMCounter.TMRuleSeq,
                                            TMQueue.MsSeq,
                                            TMQueue.DateSt,
                                            OUTPUT ldeTotalDataLimit).
      IF ldeTotalDataLimit > 0 AND
         TMCounter.LimitAmt NE ldeTotalDataLimit THEN
      ASSIGN
         TMCounter.IntValue = liUpSellCount
         TMCounter.LimitAmt = ldeTotalDataLimit.
   END.

   RETURN llIsDataBundleLimit.
END.

FUNCTION fDSSUpsellBundleLimitNew RETURN LOGICAL
   ( INPUT iiTMRuleSeq AS INT):

   DEF VAR llIsDataBundleLimit AS LOG NO-UNDO.
   DEF VAR ldeTotalDataLimit   AS DEC NO-UNDO.

   llIsDataBundleLimit = (LOOKUP(STRING(iiTMRuleSeq),lcDSSUpsell) > 0).

   IF llIsDataBundleLimit AND TMCounter.LimitAmt = 0 THEN DO:
      liUpSellCount = fGetDSSUpsellBundleCount(TMCounter.TMRuleSeq,
                                               TMQueue.CustNum,
                                               TMQueue.DateSt,
                                               OUTPUT ldeTotalDataLimit).
      IF ldeTotalDataLimit >= 0 AND
         TMCounter.LimitAmt <> ldeTotalDataLimit THEN DO:

         IF TMCounter.LimitAmt < ldeTotalDataLimit AND
            TMCounter.Amount < ldeTotalDataLimit THEN
            TMCounter.LimitID = 0.

         ASSIGN
            TMCounter.IntValue = liUpSellCount
            TMCounter.LimitAmt = ldeTotalDataLimit.
      END.
   END.

   RETURN llIsDataBundleLimit.
END.


FUNCTION fIsUpsellBundleLimit RETURN LOGICAL
   ( INPUT iiTMRuleSeq AS INT):

   RETURN LOOKUP(STRING(iiTMRuleSeq), lcUpsells) > 0.
END.

FUNCTION fAccumulateInvRowCounter RETURNS LOGIC:

   DEF VAR ldaToDate AS DATE NO-UNDO.
   DEF VAR liLockCnt AS INT  NO-UNDO.
   DEF VAR liSign    AS INT  NO-UNDO.
   DEF VAR ldaISTCFrom AS DATE NO-UNDO. 
   DEF VAR ldaFromDate AS DATE NO-UNDO. 
   
   IF TMQueue.Source = "NRTRDE" OR
      TMQueue.PayType = 2 OR 
      TMQueue.InvSeq = 0 THEN RETURN TRUE.
   
   /* always on calendar month level */
   IF MONTH(TMQueue.DateSt) = 12 THEN 
      ldaToDate = DATE(12,31,YEAR(TMQueue.DateSt)).
   ELSE ldaToDate = DATE(MONTH(TMQueue.DateSt) + 1,1,YEAR(TMQueue.DateSt)) - 1.

   FIND FIRST InvRowCounter NO-LOCK WHERE
              InvRowCounter.InvCust     = TMQueue.InvCust AND
              InvRowCounter.InvSeq      = TMQueue.InvSeq AND
              InvRowCounter.BillCode    = TMQueue.BillCode AND
              InvRowCounter.CCN         = TMQueue.CCN AND
              InvRowCounter.MsSeq       = TMQueue.MsSeq AND
              InvRowCounter.CLI         = TMQueue.CLI AND
              InvRowCounter.TariffNum   = TMQueue.TariffNum AND
              InvRowCounter.VatIncl     = TMQueue.VatIncl AND
              InvRowCounter.ReportingID = TMQueue.ReportingID AND
              InvRowCounter.DCEvent     = TMQueue.DCEvent AND
              InvRowCounter.ToDate     >= TMQueue.DateSt AND
              InvRowCounter.ToDate     <= ldaToDate 
              USE-INDEX InvCust NO-ERROR.

   /* already billed, no updates anymore */
   IF AVAILABLE InvRowCounter AND InvRowCounter.InvNum > 0 THEN DO:
      FIND FIRST Invoice WHERE Invoice.InvNum = InvRowCounter.InvNum
         NO-LOCK NO-ERROR.
      IF NOT AVAILABLE Invoice OR Invoice.InvType NE {&INV_TYPE_TEST} THEN
         RETURN TRUE.
   END.
      
   IF NOT AVAILABLE InvRowCounter THEN DO:

      ASSIGN
         ldaISTCFrom = fGetiSTCDate(TMQueue.MsSeq,
                                    TMQueue.InvCust,
                                    TMQueue.DateSt)
         ldaFromDate = DATE(MONTH(ldaToDate),1,YEAR(ldaToDate)).

      IF ldaISTCFrom NE ? THEN DO:
         IF ldaISTCFrom > TMQueue.DateSt THEN
            ldaToDate = ldaISTCFrom - 1.
         ELSE ldaFromDate = ldaISTCFrom.
      END.

      CREATE InvRowCounter.
      ASSIGN 
         InvRowCounter.InvCust     = TMQueue.InvCust
         InvRowCounter.InvSeq      = TMQueue.InvSeq
         InvRowCounter.BillCode    = TMQueue.BillCode 
         InvRowCounter.CCN         = TMQueue.CCN 
         InvRowCounter.MsSeq       = TMQueue.MsSeq 
         InvRowCounter.CLI         = TMQueue.CLI 
         InvRowCounter.TariffNum   = TMQueue.TariffNum 
         InvRowCounter.VatIncl     = TMQueue.VatIncl 
         InvRowCounter.ReportingID = TMQueue.ReportingID 
         InvRowCounter.DCEvent     = TMQueue.DCEvent
         InvRowCounter.FromDate    = ldaFromDate
         InvRowCounter.ToDate      = ldaToDate.
   END.

   ELSE REPEAT:
      FIND CURRENT InvRowCounter EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
      IF LOCKED(InvRowCounter) THEN DO:
         liLockCnt = liLockCnt + 1. 
         IF liLockCnt > 100 THEN RETURN FALSE.
         PAUSE 2 NO-MESSAGE. 
      END. 
      ELSE LEAVE.
   END.

   IF TMQueue.Qty < 0 THEN liSign = -1.
   ELSE liSign = 1. 
   
   ASSIGN
      InvRowCounter.Quantity = InvRowCounter.Quantity + TMQueue.Qty
      InvRowCounter.Duration = InvRowCounter.Duration + 
                                  TMQueue.BillDur * liSign
      InvRowCounter.Amount   = InvRowCounter.Amount + TMQueue.Amount * liSign
      InvRowCounter.DataAmt  = InvRowCounter.DataAmt +
                                  (TMQueue.DataIn + TMQueue.DataOut) * liSign
      InvRowCounter.RefPrice = InvRowCounter.RefPrice +
                                     TMQueue.RefPrice * liSign
      InvRowCounter.ExtraAmount = InvRowCounter.ExtraAmount + 
                                     TMQueue.ExtraAmount * liSign.
      
   RELEASE InvRowCounter.
   
   RETURN TRUE.  

END FUNCTION.


FUNCTION fAccumulatePrepRowCounter RETURNS LOGIC:

   DEF VAR ldaToDate AS DATE NO-UNDO.
   DEF VAR liLockCnt AS INT  NO-UNDO.
   DEF VAR liSign    AS INT  NO-UNDO.

   IF TMQueue.Source = "NRTRDE" OR
      TMQueue.PayType = 1 OR
      TMQueue.BillCode = "TopupEvent" THEN RETURN TRUE.
   
   /* always on calendar month level */
   IF MONTH(TMQueue.DateSt) = 12 THEN 
      ldaToDate = DATE(12,31,YEAR(TMQueue.DateSt)).
   ELSE ldaToDate = DATE(MONTH(TMQueue.DateSt) + 1,1,YEAR(TMQueue.DateSt)) - 1.
   
   FIND FIRST PrepRowCounter WHERE
              PrepRowCounter.InvCust  = TMQueue.InvCust AND
              PrepRowCounter.BillCode = TMQueue.BillCode AND
              PrepRowCounter.MsSeq    = TMQueue.MsSeq AND
              PrepRowCounter.CLI      = TMQueue.CLI AND
              PrepRowCounter.ToDate   = ldaToDate NO-LOCK NO-ERROR.
   IF NOT AVAILABLE PrepRowCounter THEN DO:
      CREATE PrepRowCounter.
      ASSIGN 
         PrepRowCounter.InvCust  = TMQueue.InvCust
         PrepRowCounter.BillCode = TMQueue.BillCode 
         PrepRowCounter.MsSeq    = TMQueue.MsSeq 
         PrepRowCounter.CLI      = TMQueue.CLI 
         PrepRowCounter.FromDate = DATE(MONTH(ldaToDate),1,YEAR(ldaToDate))
         PrepRowCounter.ToDate   = ldaToDate.
   END.
   ELSE REPEAT:
      FIND CURRENT PrepRowCounter EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
      IF LOCKED(PrepRowCounter) THEN DO:
         liLockCnt = liLockCnt + 1. 
         IF liLockCnt > 100 THEN RETURN FALSE.
         PAUSE 2 NO-MESSAGE. 
      END. 
      ELSE LEAVE.
   END.

   IF TMQueue.Qty < 0 THEN liSign = -1.
   ELSE liSign = 1. 
   
   ASSIGN
      PrepRowCounter.Quantity = PrepRowCounter.Quantity + TMQueue.Qty
      PrepRowCounter.Amount   = PrepRowCounter.Amount + TMQueue.Amount * liSign.
      
   RELEASE PrepRowCounter.
   
   RETURN TRUE.  

END FUNCTION.


/********* Main start ********/

ASSIGN
   lhSource   = BUFFER TMQueue:HANDLE
   lcStarted  = fTS2HMS(fMakeTS())
   lcLogFile  = fCParamC("TMQueueDeleteLog")
   liInterval = fCParamI("TMQueueInterval")
   liTransQty = fCParamI("TMQueueTransQty")
   lcCONTFVoice100 = fCParamC("TMQueueCONTFVoice")
   liTotalTrafficFraudSeq = fCParamI("TMQueueTTFSeq")
   lcUpsellsNew = lcIPLData + "," + lcBonoData + "," +
                  lcPMDUBUpsell + "," + lcBaseContracts
   lcUpsells  = lcUpsellsNew + "," + lcDSSUpsell
   llMonitor  = TRUE
   lcMonitor  = "COUNTERS:TMQueue".

FIND FIRST TMRule NO-LOCK WHERE
           TMRule.Name EQ "Premium Fraud" NO-ERROR.

IF NOT AVAIL TMRule THEN DO:
   MESSAGE "Missing configurations" VIEW-AS ALERT-BOX.
   QUIT.
END.
ELSE liPremiumFraudPostSeq = TMRule.TMRuleSeq.

FIND FIRST TMRule NO-LOCK WHERE
           TMRule.Name EQ "Premium Fraud Prepaid" NO-ERROR.

IF NOT AVAIL TMRule THEN DO:
   MESSAGE "Missing configurations" VIEW-AS ALERT-BOX.
   QUIT.
END.
ELSE liPremiumFraudPreSeq = TMRule.TMRuleSeq.
   
IF liInterval = ? THEN liInterval = 10.
IF liTransQty = ? THEN liTransQty = 2000.
IF lcLogFile = ? THEN lcLogFile = "".

lcLogFile = REPLACE(lcLogFile,"#DATE",STRING(TODAY,"999999")).

/* called from another module with a list of InvSeqs */
lcInvSeqList = SOURCE-PROCEDURE:PRIVATE-DATA NO-ERROR.
IF lcInvSeqList BEGINS "TMQueue_Param:" THEN DO:
   ASSIGN
      lcInvSeqList = ENTRY(2,lcInvSeqList,":")
      llMonitor    = FALSE.
   IF lcInvSeqList = "" THEN RETURN.
END.
ELSE lcInvSeqList = "".


MainLoop:
DO WHILE TRUE:
    
   liDays = 0.
               
   /* get latest configuration once a day */
   IF ldtUpdated = ? THEN liDays = 1.
   ELSE DO:
      IF TODAY > ldtUpdated THEN liDays = 1.                          
   END.
   
   if liDays >= 1 THEN DO:
      RUN pUpdateTempTables.
      
      IF NOT CAN-FIND(FIRST ttItems) THEN DO:
         MESSAGE "No configuration rules available"
         VIEW-AS ALERT-BOX ERROR.
         QUIT.
      END.
   END.

   /* write deleted queue rows to log */                                
   IF lcLogFile > "" THEN 
      OUTPUT STREAM sDel TO VALUE(lcLogFile) APPEND.

   IF lcInvSeqList = "" THEN DO:

      QueueLoop:
      FOR EACH bQueue NO-LOCK
         liLoop = 1 TO liTransQty TRANS:

         ldQty = ldQty + 1.

         FIND TMQueue WHERE RECID(TMQueue) = RECID(bQueue) EXCLUSIVE-LOCK
            NO-WAIT NO-ERROR.
         IF LOCKED(TMQueue) THEN NEXT.
         IF NOT AVAIL TMQueue THEN NEXT.

         RUN pAnalyseQueueRow.
         IF RETURN-VALUE BEGINS "UNDO" THEN UNDO QueueLoop, NEXT QueueLoop. 
      END.  
   END.
   
   ELSE DO liInvSeqCnt = 1 TO NUM-ENTRIES(lcInvSeqList):
   
      liInvSeq = INTEGER(ENTRY(liInvSeqCnt,lcInvSeqList)) NO-ERROR.
      IF ERROR-STATUS:ERROR OR liInvSeq = 0 THEN NEXT.
      
      InvSeqLoop:
      FOR EACH bQueue NO-LOCK WHERE
               bQueue.InvSeq = liInvSeq
         liLoop = 1 TO liTransQty TRANS:

         ldQty = ldQty + 1.

         FIND TMQueue WHERE RECID(TMQueue) = RECID(bQueue) EXCLUSIVE-LOCK
            NO-WAIT NO-ERROR.
         IF LOCKED(TMQueue) THEN NEXT.
         IF NOT AVAIL TMQueue THEN NEXT.

         RUN pAnalyseQueueRow.
         IF RETURN-VALUE BEGINS "UNDO" THEN UNDO InvSeqLoop, NEXT InvSeqLoop. 
      END.  
   END.
    
   IF lcLogFile > "" THEN 
      OUTPUT STREAM sDel CLOSE.
 
   IF lcInvSeqList > "" THEN DO:
      IF ldPrevQty = ldQty THEN LEAVE MainLoop.
      ldPrevQty = ldQty.
      NEXT MainLoop.
   END.
 
   /* monitoring */
   IF llMonitor THEN DO:
      PUT SCREEN ROW 23 COL 2 "Monitor " + lcMonitor.

      fKeepAlive(lcMonitor).
   END.
   
   lcCurrent = fTS2HMS(fMakeTS()).
        
   PAUSE 0.
   DISP lcStarted
        lcTempUpdated
        ldQty    
        ldtLatest
        ldDone 
        ldCounter 
        liActions
        lcCurrent 
   WITH FRAME fQty.

   PUT SCREEN ROW 22 COL 2
      "F8 TO QUIT, other keys start handling immediatelly (Interval=" +
      STRING(liInterval) + ")".
 
   READKEY PAUSE liInterval.
    
   IF LOOKUP(KEYLABEL(LASTKEY),"F8,8") > 0 THEN DO:
       LEAVE MainLoop.
   END.

   PUT SCREEN ROW 22 COL 2 FILL(" ",70).

END.  /* MainLoop */

DELETE OBJECT lhField.
DELETE OBJECT lhSource. 
IF VALID-HANDLE(ghFunc1) THEN DELETE OBJECT ghFunc1 NO-ERROR.

IF lcInvSeqList = "" THEN QUIT.

/********* Main end ********/


PROCEDURE pAnalyseQueueRow:   

   DEF VAR llNewBundleLimit AS LOG NO-UNDO. 
   DEF VAR ldeTimeStamp AS DEC NO-UNDO. 
   DEF VAR ldaFrom AS DATE NO-UNDO. 
 
   ASSIGN
      ldtLatest     = TMQueue.DateSt
      llBarringDone = FALSE
      liTicketType = (IF TMQueue.Source = "NRTRDE"
                      THEN {&TICKET_TYPE_FRAUD}
                      ELSE {&TICKET_TYPE_MOBILE}).
      
   RuleLevel:
   FOR EACH ttRule WHERE 
            ttRule.PayType = TMQueue.PayType AND
            ttRule.CounterType > 0 AND
            ttRule.TicketType = liTicketType:

      /* specific targets defined for this event */
      IF TMQueue.AccumTarget > "" THEN DO:
         IF LOOKUP("TMR:" + STRING(ttRule.TMRuleSeq),
                   TMQueue.AccumTarget) = 0 THEN NEXT RuleLevel.
      END.
    
      /* triggered only once */
      IF ttRule.CounterType = 3 AND
         CAN-FIND(FIRST TMCounter WHERE
                        TMCounter.MsSeq     = TMQueue.MsSeq    AND
                        TMCounter.TMRuleSeq = ttRule.TMRuleSeq AND
                        TMCounter.LimitID  >= ttRule.MaxLimits)
      THEN NEXT. 
      ELSE IF ttRule.CounterType = 1 AND
              TMQueue.Bdest = "GPRSDATA2" AND
              fIsUpsellBundleLimit(ttRule.TMRuleSeq) AND
            CAN-FIND(FIRST TMCounter WHERE
                           TMCounter.MsSeq     = TMQueue.MsSeq    AND
                           TMCounter.TMRuleSeq = ttRule.TMRuleSeq AND
                           TMCounter.LimitID   = ttRule.MaxLimits AND
                           TMCounter.Amount   >= TMCounter.LimitAmt)
      THEN NEXT. 
         
      ItemLevel: 
      FOR EACH ttItems WHERE
               ttItems.TMRuleSeq = ttRule.TMRuleSeq:
        
         llMatch = TRUE.
            
         /* special case; topup event */
         IF ttRule.PayType = 2 AND TMQueue.BillCode = "TopupEvent" THEN DO:
            IF NOT ttRule.UseTopup THEN LEAVE ItemLevel.
         END.
            
         /* do queue values match to configuration */
         FieldCheck:           
         DO liField = 1 TO NUM-ENTRIES(ttRule.CounterItems):

            IF ENTRY(liField,ttItems.Items) = "*" THEN NEXT.
         
            ASSIGN
               lcCounterItem = ENTRY(liField, ttRule.CounterItems)
               lcCounterItemValue = "".

            /* special handling required */
            IF lcCounterItem BEGINS "#" THEN DO:

               CASE lcCounterItem:

               WHEN "#FraudGroup" THEN DO:
                          
                  FIND FIRST Mobsub WHERE MobSub.MsSeq = TMQueue.MSSeq 
                     NO-LOCK NO-ERROR.
                  IF NOT AVAIL mobsub THEN DO:
                     llMatch = FALSE.
                     LEAVE FieldCheck.
                  END.

                  /* currently only fraud cdrs are supported  */
                  FIND FIRST FraudCDR WHERE
                             FraudCDR.CLI    = Mobsub.CLI    AND 
                             FraudCDR.datest = TMQueue.Datest AND 
                             FraudCDR.dtlseq = TMQueue.EventId 
                     NO-LOCK NO-ERROR.
                        
                  IF NOT AVAIL FraudCDR THEN DO:
                     llMatch = FALSE.
                     LEAVE FieldCheck.
                  END.

                  lcCountry = fDestCountry(gcBrand,
                                           TMQueue.SpoCMT,
                                           TMQueue.DateSt,
                                           TMQueue.EventId,
                                           FraudCDR.GSMBnr,
                                           FraudCDR.BType,
                                           TMQueue.Source).

                  FIND Country WHERE
                       Country.Country = lcCountry NO-LOCK NO-ERROR.
                  IF AVAIL Country AND Country.Country NE "ES" THEN DO:   
                     lcCounterItemValue = Country.FraudGroup.
                     IF lcCounterItemValue = "" THEN
                        lcCounterItemValue = "C".
                  END.
                  ELSE DO:
                     llMatch = FALSE.
                     LEAVE FieldCheck.
                  END.
               END.
               
               /* special handling not implemented */
               OTHERWISE DO:
                  llMatch = FALSE.
                  LEAVE FieldCheck.
               END.
               END CASE.
            END.
            
            ELSE DO:
               lhField = lhSource:BUFFER-FIELD(lcCounterItem).
               lcCounterItemValue = STRING(lhField:BUFFER-VALUE).
            END.
                  
            IF lcCounterItemValue NE ENTRY(liField,ttItems.Items) THEN DO:
            
               IF lcCounterItemValue MATCHES ENTRY(liField,ttItems.Items) THEN 
                  NEXT FieldCheck.
               
               llMatch = FALSE.
               LEAVE FieldCheck.
            END.
         END.
             
         /* elements of the queue row match to configuration -> 
            accumulate to a counter */       
         IF llMatch THEN DO:
           
            FIND FIRST ttPeriod WHERE
                       ttPeriod.TMRuleSeq    = ttRule.TMRuleSeq AND
                       ttPeriod.PeriodEnd   >= TMQueue.DateSt   AND
                       ttPeriod.PeriodBegin <= TMQueue.DateSt NO-ERROR.
                          
            IF AVAILABLE ttPeriod THEN DO:
               
               IF STRING(ttRule.TMRuleSeq) EQ lcDSSUpsell THEN
               FIND FIRST TMCounter USE-INDEX Custnum WHERE
                          TMCounter.Custnum   = TMQueue.Custnum AND
                          TMCounter.TMRuleSeq = ttRule.TMRuleSeq AND
                          TMCounter.ToDate    = ttPeriod.PeriodEnd AND
                          TMCounter.FromDate  = ttPeriod.PeriodBegin AND
                          TMCounter.CustNum   = TMQueue.CustNum
               EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
               ELSE
               FIND FIRST TMCounter USE-INDEX MsSeq WHERE
                          TMCounter.MsSeq     = TMQueue.MsSeq      AND
                          TMCounter.TMRuleSeq = ttRule.TMRuleSeq   AND
                          TMCounter.ToDate    = ttPeriod.PeriodEnd AND
                          TMCounter.FromDate >= ttPeriod.PeriodBegin AND
                          TMCounter.CustNum   = TMQueue.CustNum
               EXCLUSIVE-LOCK NO-WAIT NO-ERROR.

               /* counter exists but cannot be updated -> try again
                  later */
               IF LOCKED(TMCounter) THEN RETURN "UNDO".
                   
               IF NOT AVAILABLE TMCounter THEN DO:

                  IF STRING(ttRule.TMRuleSeq) EQ lcBaseContracts THEN DO:
                     ldaFrom = fGetiSTCDate(TMQueue.MsSeq,
                                            TMQueue.InvCust,
                                            TMQueue.DateSt).
                     
                     IF ldaFrom EQ ? THEN ldaFrom = ttPeriod.PeriodBegin.
                     ELSE IF ldaFrom > TMQueue.DateSt THEN LEAVE ItemLevel.

                  END.
                  ELSE ldaFrom = ttPeriod.PeriodBegin.
                  
                  ASSIGN 
                     ldeMonthBegin = 0
                     ldeMonthEnd   = 0
                     ldeTotalLimit = 0
                     ldeMonthBegin = fMake2Dt(ldaFrom,0)
                     ldeMonthEnd   = fMake2Dt(ttPeriod.PeriodEnd,86399).

                  /* YDR-2109 Get Total fraud Limit Counter value for 
                     New Bundles, upsells and roaming upsells */
                  IF ttRule.TMRuleSeq EQ liTotalTrafficFraudSeq THEN 
                     fGetTotalBundleUsage (TMQueue.MsSeq,
                                           TMQueue.CustNum,
                                           ldeMonthBegin,
                                           ldeMonthEnd,
                                           OUTPUT ldeTotalLimit).
                  CREATE TMCounter.
                  ASSIGN
                     TMCounter.MsSeq     = TMQueue.MsSeq WHEN 
                        STRING(ttRule.TMRuleSeq) NE lcDSSUpsell
                     TMCounter.CustNum   = TMQueue.CustNum
                     TMCounter.TMRuleSeq = ttRule.TMRuleSeq
                     TMCounter.FromDate  = ldaFrom
                     TMCounter.ToDate    = ttPeriod.PeriodEnd
                     TMCounter.Amount    = ldeTotalLimit.
               END.      
                  
               IF TMCounter.FromDate > TMQueue.DateSt THEN LEAVE ItemLevel.

               /* Include CONTF, CONTFF monthly fee to total traffic fraud */
               IF TMQueue.CLIType BEGINS "CONTF" AND
                  ttRule.TMRuleSeq EQ liTotalTrafficFraudSeq AND
                  TMCounter.DecValue = 0 THEN DO:

                  ldeTimeStamp = fMake2Dt(TMQueue.DateST, 86399).
                  
                  FIND FIRST MobSub NO-LOCK WHERE
                             MobSub.MsSeq = TMQueue.MsSeq NO-ERROR.
                  IF AVAIL MobSub AND
                           MobSub.CLIType BEGINS "CONTF" AND
                     NOT CAN-FIND(FIRST MsRequest NO-LOCK WHERE /* YBU-1887 */
                                        MsRequest.MsSeq = MobSub.MsSeq AND
                                        MsRequest.ReqType = 0 AND
                                        MsRequest.Actstamp < ldeTimeStamp AND
                          LOOKUP(STRING(MsRequest.ReqStatus),
                                 {&REQ_INACTIVE_STATUSES}) = 0
                          USE-INDEX MsSeq) AND
                     NOT CAN-FIND(FIRST MsRequest NO-LOCK WHERE 
                                        MsRequest.MsSeq = MobSub.MsSeq AND
                                        MsRequest.ReqType = 
                                          {&REQTYPE_BUNDLE_CHANGE} AND
                                        MsRequest.ReqCParam1 BEGINS "CONTF" AND
                                        MsRequest.ReqCParam2 BEGINS "CONTF" AND
                                        MsRequest.Actstamp < ldeTimeStamp AND
                          LOOKUP(STRING(MsRequest.ReqStatus),
                                 {&REQ_INACTIVE_STATUSES}) = 0
                          USE-INDEX MsSeq) THEN DO:
                  
                     ldeAmount = fGetCONTFFraudCounterFee(
                                    MobSub.MsSeq,
                                    fMake2Dt(TMQueue.DateST,86399)).
                     IF ldeAmount > 0 THEN ASSIGN
                        TMCounter.Amount = TMCounter.Amount + ldeAmount
                        TMCounter.DecValue = ldeAmount.
                  END.
               END.

               CASE ttRule.CounterAmount:
               /* minutes */
               WHEN "1" THEN ldValue = TMQueue.BillDur / 60.
               /* seconds */
               WHEN "2" THEN ldValue = TMQueue.BillDur.
               /* gigabytes (unit of data in cdrs is one byte) */
               WHEN "3" THEN DO:
                  /* Prepaid contain data as KB */
                  IF TMQueue.PayType = 2 THEN
                     ldValue = (TMQueue.DataIn + TMQueue.DataOut) /
                                (1024 * 1024).
                  ELSE
                     ldValue = (TMQueue.DataIn + TMQueue.DataOut) /
                                (1024 * 1024 * 1024).
               END. /* WHEN "3" THEN DO: */
               /* megabytes */
               WHEN "4" THEN DO:
                  /* Prepaid contain data as KB */
                  IF TMQueue.PayType = 2 THEN
                     ldValue = (TMQueue.DataIn + TMQueue.DataOut) / 1024.
                  ELSE
                     ldValue = (TMQueue.DataIn + TMQueue.DataOut) /
                                (1024 * 1024).
               END. /* WHEN "4" THEN DO: */
               /* qty */
               WHEN "5" THEN ldValue = 1.
               /* price */
               WHEN "6" THEN ldValue = TMQueue.Amount.
               /* BDest Limit */
               WHEN "7" THEN DO:
                  ldValue = 0.
                  TMCounter.Amount = fGetBDestCount(TMQueue.MsSeq,
                                                    TMQueue.DCEvent,
                                                    TMQueue.DateST).
               END. /* WHEN "7" THEN DO: */
               OTHERWISE ldValue = 0.
               END CASE. 
                  
               IF ttRule.LimitCompare = 2 THEN ldValue = ldValue * -1.
               
               IF TMQueue.Qty < 0 THEN ldValue = ldValue * TMQueue.Qty.
 
               ldCounter = ldCounter + 1.

               IF STRING(TMCounter.TMRuleSeq) EQ lcDSSUpsell AND
                  NEW(TMCounter) THEN DO:
                  TMCounter.Amount = fGetAllDSSUsage(TMCounter.TMRuleSeq,
                                                     TMCounter.CustNum,
                                                     TMQueue.DateST).
                  TMCounter.Amount = TMCounter.Amount / 1024 / 1024.
               END.
               ELSE
                  TMCounter.Amount = TMCounter.Amount + ldValue.
                  
               IF ttRule.LimitSource > 0 THEN DO:
                  liUpSellCount = ?.

                  IF STRING(TMCounter.TMRuleSeq) EQ lcDSSUpsell THEN
                     llNewBundleLimit = fDSSUpsellBundleLimitNew(TMCounter.TMRuleSeq).
                  ELSE
                     llNewBundleLimit = fUpsellBundleLimitNew(TMCounter.TMRuleSeq).

                  /* check if new value exceeds given limits */
                  RUN pCheckLimits (ttRule.LimitSource,
                                    IF TMQueue.AgrCust > 0
                                    THEN TMQueue.AgrCust
                                    ELSE TMQueue.InvCust,
                                    ttRule.LimitCompare,
                                    TMQueue.Bdest,
                                    liUpSellCount,
                                    OUTPUT liLimitID). 
                                    
                  /* trigger the action if limit was exceeded */ 
                  IF liLimitID > 0 THEN DO:
                        
                     /* Special check for upsell data bundle limits */
                     IF llNewBundleLimit THEN DO:
                        /* Maximum upsell are configured in TMS */
                        IF TMCounter.IntValue < 10 THEN
                           RUN pLimitAction(liLimitID).
                        ELSE DO:
                           FIND CURRENT TMCounter EXCLUSIVE-LOCK.
                           ASSIGN
                              TMCounter.LimitID = liLimitID.
                        END.
                     END.
                     ELSE RUN pLimitAction(liLimitID).
                     
                     /* couldn't be performed, undo whole counter handling
                        and try again in next loops */
                     IF RETURN-VALUE BEGINS "UNDO" THEN RETURN "UNDO".
                  END.   
               END.
                     
               RELEASE TMCounter.
                        
            END.
            
            LEAVE ItemLevel.
         END.
               
      END.  /* ItemLevel */

   END.  /* RuleLevel */

   /* accumulate invoice row counters */
   IF TMQueue.AccumTarget = "" OR 
      LOOKUP("InvRow",TMQueue.AccumTarget) > 0 THEN DO:
      IF TMQueue.PayType = 1 THEN DO:
         IF NOT fAccumulateInvRowCounter() THEN RETURN "UNDO".
      END.
      ELSE IF TMQueue.PayType = 2 THEN DO:
         IF NOT fAccumulatePrepRowCounter() THEN RETURN "UNDO".
      END.
   END.
 
   ldDone = ldDone + 1.
    
   IF ldQty MOD 100 = 0 AND lcInvSeqList = "" THEN DO:
      lcCurrent = fTS2HMS(fMakeTS()).
        
      PAUSE 0.
      DISP ldQty    
           ldtLatest
           ldDone 
           ldCounter 
           liActions
           lcCurrent 
      WITH FRAME fQty.
   END.

   IF lcLogFile > "" THEN   
      EXPORT STREAM sDel TMQueue.
      
   DELETE TMQueue.
 
   RETURN "".
      
END PROCEDURE. 

/* get latest configurations */
PROCEDURE pUpdateTempTables:

   DEF VAR liAster     AS INT  NO-UNDO.
   DEF VAR liBegin     AS INT  NO-UNDO.
   DEF VAR ldtDate     AS DATE NO-UNDO.
   DEF VAR liMonth     AS INT  NO-UNDO.
   DEF VAR lcField     AS CHAR NO-UNDO.
   DEF VAR lcFieldList AS CHAR NO-UNDO.
   DEF VAR lcConfLog   AS CHAR NO-UNDO.
   
   EMPTY TEMP-TABLE ttRule.
   EMPTY TEMP-TABLE ttItems.
   EMPTY TEMP-TABLE ttPeriod.
   EMPTY TEMP-TABLE ttLimits.
   
   DEF BUFFER bttItems FOR ttItems.

   /* get possible field names */
   DO liBegin = 1 TO lhSource:NUM-FIELDS:
      lhField = lhSource:BUFFER-FIELD(liBegin).
      lcFieldList = lcFieldList + (IF liBegin > 1 THEN "," ELSE "") + 
                    lhField:NAME.
   END.
 
   GetRules:
   FOR EACH TMRule NO-LOCK WHERE
            TMRule.Brand     = gcBrand AND
            TMRule.FromDate <= TODAY   AND
            TMRule.ToDate   >= TODAY:
            
      /* check that all field names are valid */
      DO liBegin = 1 TO NUM-ENTRIES(TMRule.CounterItems):
         lcField = ENTRY(liBegin,TMRule.CounterItems).
         IF NOT lcField BEGINS "#" AND LOOKUP(lcField,lcFieldList) = 0 
         THEN DO:
            MESSAGE "Invalid field name" lcField 
                    "in rule" TMRule.TMRuleSeq TMRule.Name
            VIEW-AS ALERT-BOX ERROR.
            NEXT GetRules.
         END.
      END.

      CREATE ttRule.
      BUFFER-COPY TMRule TO ttRule.
      ttRule.UseTopup = FALSE.

      /* period for the counters */
      CASE TMRule.CounterPeriod:

      /* no period separation */
      WHEN 0 THEN DO:
         IF ttRule.CounterType = 3 THEN DO:
            CREATE ttPeriod.
            ASSIGN
               ttPeriod.TMRuleSeq   = ttRule.TMRuleSeq
               ttPeriod.PeriodBegin = ttRule.FromDate
               ttPeriod.PeriodEnd   = ttRule.ToDate.
         END.
      END.
      
      /* calendar month */
      WHEN 2 THEN DO:

         liMonth = 0.   
         DO ldtDate = TODAY - 180 TO TODAY + 40:
               
            IF Month(ldtDate) NE liMonth THEN DO:
               CREATE ttPeriod.
               ASSIGN
                  ttPeriod.TMRuleSeq  = ttRule.TMRuleSeq
                  ttPeriod.PeriodBegin = DATE(MONTH(ldtDate),1,YEAR(ldtDate)).

               IF MONTH(ldtDate) = 12
               THEN ttPeriod.PeriodEnd = DATE(12,31,YEAR(ldtDate)).
               ELSE ttPeriod.PeriodEnd = DATE(MONTH(ldtDate) + 1,1,
                                              YEAR(ldtDate)) - 1.
               liMonth = MONTH(ldtDate).
            END.
         END.    
      END.

      END CASE.
      
      FOR EACH TMRItemValue OF TMRule NO-LOCK WHERE
               TMRItemValue.FromDate <= TODAY AND
               TMRItemValue.ToDate   >= TODAY:
               
         /* there must be the same qty of item values that there are
            items on the rule */
         IF NUM-ENTRIES(TMRItemValue.CounterItemValues) NE
            NUM-ENTRIES(TMRule.CounterItems)
         THEN NEXT.
             
         CREATE ttItems.
         ASSIGN 
            ttItems.TMRuleSeq = TMRItemValue.TMRuleSeq
            ttItems.Items     = TMRItemValue.CounterItemValues.

         /* special handling for topup events */
         IF TMRule.PayType = 2 AND 
            LOOKUP("TopupEvent",TMRItemValue.CounterItemValues) > 0
         THEN DO:
            ttRule.UseTopup = TRUE.
            ttItems.Items = REPLACE(ttItems.Items,"TopupEvent","*").
         END.

         IF CAN-FIND(FIRST bttItems WHERE
                           bttItems.TMRuleSeq = ttItems.TMRuleSeq AND
                           bttItems.Items     = ttItems.Items AND
                           RECID(bttItems) NE RECID(ttItems))
         THEN DO:
            DELETE ttItems.
            NEXT.
         END.
                           
      END.      

      FOR EACH TMRLimit OF TMRule NO-LOCK WHERE
               TMRLimit.FromDate <= TODAY AND
               TMRLimit.ToDate   >= TODAY:

         CREATE ttLimits.
         BUFFER-COPY TMRLimit TO ttLimits.
         
         ttRule.MaxLimits = MAX(ttRule.MaxLimits,TMRLimit.LimitID).
      END.  

      /* YPR-1425 */
      IF TMRule.TMRuleSeq EQ 1 THEN DO:
         CREATE ttLimits.
         ASSIGN
            ttLimits.Action       = 35
            ttLimits.ActionParam  = "Y_HURP_P"
            ttLimits.FromDate     = 8/1/2014
            ttLimits.LimitAmt     = {&PREMIUM_BARRING_LIMIT} 
            ttLimits.LimitID      = 3
            ttLimits.SMSText      = "PREMIUMFRAUD3"
            ttLimits.TMRuleSeq    = liPremiumFraudPostSeq
            ttLimits.ToDate       = 12/21/2049
            ttLimits.ValueType    = 1.
      END.
   END.
   
   ASSIGN
      ldTempUpdated = fMakeTS()
      lcTempUpdated = fTS2HMS(ldTempUpdated)
      ldtUpdated    = TODAY
      lcConfLog     = fCParamC("TMQueueConfLog").

   IF lcConfLog > "" THEN DO:
      OUTPUT STREAM sLog TO VALUE(lcConfLog) APPEND.
      PUT STREAM sLog UNFORMATTED
         "Configuration updated " 
         STRING(TODAY,"99.99.99") SPACE(1)
         STRING(TIME,"hh:mm:ss") SKIP.
      OUTPUT STREAM sLog CLOSE.
   END.
    
   /* statistics */
   IF ldDone > 0 THEN DO TRANS:
      CREATE ActionLog.
      ASSIGN 
         ActionLog.Brand        = gcBrand   
         ActionLog.TableName    = "Analysis"  
         ActionLog.KeyValue     = STRING(TODAY,"99.99.99") 
         ActionLog.ActionID     = "TMQUEUE"
         ActionLog.ActionPeriod = YEAR(TODAY) * 100 + MONTH(TODAY)
         ActionLog.ActionDec    = ldCounter
         ActionLog.ActionChar   = 
            "Started: " + lcStarted + CHR(10) + 
            "Handled: " + STRING(ldDone) + CHR(10) +
            "To Counters: " + STRING(ldCounter) + CHR(10) + 
            "Actions: " + STRING(liActions) 
         ActionLog.ActionStatus = 3
         ActionLog.ActionTS     = ldTempUpdated.
      RELEASE ActionLog.   
   END. 
   
END PROCEDURE.

PROCEDURE pCheckLimits:

   DEF INPUT  PARAMETER iiLimitSource  AS INT  NO-UNDO.
   DEF INPUT  PARAMETER iiCustNum      AS INT  NO-UNDO.
   DEF INPUT  PARAMETER iiLimitCompare AS INT  NO-UNDO.
   DEF INPUT  PARAMETER icBDest        AS CHAR NO-UNDO.
   DEF INPUT  PARAMETER iiUpsells      AS INT  NO-UNDO.
   DEF OUTPUT PARAMETER oiLimitID      AS INT  NO-UNDO.
   
   DEF VAR ldePercentageUsed AS DEC NO-UNDO. 
   DEF VAR liPeriod    AS INT NO-UNDO.
   DEF VAR ldeActStamp AS DEC NO-UNDO. 

   oiLimitID = 0.
      
   /* use customer limit if it is found, otherwise use default limit */
   IF iiLimitSource = 4 THEN DO:
      IF CAN-FIND(FIRST Limit NO-LOCK USE-INDEX CustNum WHERE
                        Limit.CustNum   = iiCustNum           AND
                        Limit.LimitType = 1                   AND
                        Limit.TMRuleSeq = TMCounter.TMRuleSeq AND
                        Limit.ToDate   >= TODAY               AND
                        Limit.FromDate <= TODAY)
      THEN iiLimitSource = 1.
      ELSE iiLimitSource = 3.
   END.
   
   /* which limit is used */
   CASE iiLimitSource:

   /* customer based */
   WHEN 1 THEN DO:

      /* limit changes should become effective immediately
           -> temp-table not used for limits  
        
         no action in either case; 
         - customer has a limit but counter amount doesn't exceed it
         - customer has no limit record (general limit will not be used)
      */
      IF iiLimitCompare = 2 THEN 
      FOR EACH Limit NO-LOCK USE-INDEX CustNum WHERE
               Limit.CustNum   = iiCustNum           AND
               Limit.LimitType = 1                   AND
               Limit.TMRuleSeq = TMCounter.TMRuleSeq AND
               Limit.ToDate   >= TODAY               AND
               Limit.FromDate <= TODAY               AND
               Limit.LimitAmt >= TMCounter.Amount 
      BY Limit.LimitAmt:         
         oiLimitID = Limit.LimitID.
         LEAVE.
      END.

      ELSE DO:
         FOR EACH Limit NO-LOCK USE-INDEX CustNum WHERE
                  Limit.CustNum   = iiCustNum           AND
                  Limit.LimitType = 1                   AND
                  Limit.TMRuleSeq = TMCounter.TMRuleSeq AND
                  Limit.ToDate   >= TODAY               AND
                  Limit.FromDate <= TODAY               AND
                  Limit.LimitAmt <= TMCounter.Amount 
         BY Limit.LimitAmt DESC:         
            oiLimitID = Limit.LimitID.
            LEAVE.
         END.
         /* YPR-1425 */
         IF TMCounter.TMRuleSeq EQ liPremiumFraudPostSeq AND
            TMCounter.Amount >= {&PREMIUM_BARRING_LIMIT} AND
            oiLimitID EQ 0 THEN
            oiLimitID = 3.
      END.
   END.

   /* subscription based */
   WHEN 2 THEN DO:

      IF iiLimitCompare = 2 THEN 
      FOR EACH Limit NO-LOCK USE-INDEX MsSeq WHERE
               Limit.MsSeq     = TMCounter.MsSeq     AND
               Limit.LimitType = 1                   AND
               Limit.TMRuleSeq = TMCounter.TMRuleSeq AND
               Limit.ToDate   >= TODAY               AND
               Limit.FromDate <= TODAY               AND
               Limit.LimitAmt >= TMCounter.Amount 
      BY Limit.LimitAmt:         
         oiLimitID = Limit.LimitID.
         LEAVE.
      END.

      ELSE 
      FOR EACH Limit NO-LOCK USE-INDEX MsSeq WHERE
               Limit.MsSeq     = TMCounter.MsSeq     AND
               Limit.LimitType = 1                   AND
               Limit.TMRuleSeq = TMCounter.TMRuleSeq AND
               Limit.ToDate   >= TODAY               AND
               Limit.FromDate <= TODAY               AND
               Limit.LimitAmt <= TMCounter.Amount 
      BY Limit.LimitAmt DESC:         
         oiLimitID = Limit.LimitID.
         LEAVE.
      END.
   END.
   
   /* default limit from rule */
   WHEN 3 THEN DO:
      IF iiLimitCompare = 2 THEN 
      FOR EACH ttLimits WHERE
               ttLimits.TMRuleSeq = TMCounter.TMRuleSeq AND
               ttLimits.ToDate   >= TODAY               AND
               ttLimits.FromDate <= TODAY               AND
               ttLimits.LimitAmt >= TMCounter.Amount 
      BY ttLimits.LimitAmt:         
         oiLimitID = ttLimits.LimitID.
         LEAVE.
      END.

      ELSE DO:
         IF LOOKUP(STRING(TMCounter.TMRuleSeq),lcCONTFVoice100) > 0 THEN DO:

            /* Initialize limit amount */
            IF TMCounter.LimitAmt = 0 THEN DO:
               
               liPeriod = YEAR(TMQueue.DateST) * 100 + MONTH(TMQueue.DateST).
               ldeActStamp = fHMS2TS(fLastDayOfMonth(TMQueue.DateST),"23:59:59").

               FOR FIRST ServiceLimit WHERE
                         ServiceLimit.GroupCode = TMQueue.DCEvent AND
                         ServiceLimit.DialType = 4 NO-LOCK,
                   FIRST MServiceLimit NO-LOCK WHERE
                         MServiceLimit.MsSeq = TMQueue.MsSeq AND
                         MServiceLimit.DialType = ServiceLimit.DialType AND
                         MServiceLimit.SlSeq   = ServiceLimit.SlSeq AND
                         MServiceLimit.FromTS <= ldeActStamp AND
                         MServiceLimit.EndTS  >= ldeActStamp:
                  TMCounter.LimitAmt = MServiceLimit.InclAmt.
               END.
            END.
            
            ldePercentageUsed = (TMCounter.Amount / TMCounter.LimitAmt) * 100.
            
            /* it is supposed there's only one limit configured */
            FOR EACH ttLimits NO-LOCK WHERE
                     ttLimits.TMRuleSeq = TMCounter.TMRuleSeq AND
                     ttLimits.ToDate   >= TODAY               AND
                     ttLimits.FromDate <= TODAY               AND
                     ttLimits.LimitPerc <= ldePercentageUsed BY
                     ttLimits.LimitPerc DESC:
               oiLimitID = ttLimits.LimitID.
               LEAVE.
            END.

         END.
         ELSE IF LOOKUP(STRING(TMCounter.TMRuleSeq),lcUpsells) > 0 THEN DO:
               
            IF TMCounter.LimitAmt > 0 THEN DO:

               /* Assign maximum bundle limit if limit is exceed for PMDUB */
               IF icBDest EQ "GPRSDATA2" AND
                  fIsUpsellBundleLimit(TMCounter.TMRuleSeq) AND
                  TMCounter.Amount > TMCounter.LimitAmt THEN
                  TMCounter.Amount = TMCounter.LimitAmt.

               ldePercentageUsed = (TMCounter.Amount / TMCounter.LimitAmt) * 100.

               /* it is supposed there's only one limit configured */
               FIND FIRST ttLimits NO-LOCK WHERE
                          ttLimits.TMRuleSeq = TMCounter.TMRuleSeq AND
                          ttLimits.ToDate   >= TODAY               AND
                          ttLimits.FromDate <= TODAY               AND
                          ttLimits.LimitPerc <= ldePercentageUsed NO-ERROR.
               IF AVAIL ttLimits THEN
                  oiLimitID = ttLimits.LimitID.
            END.
            ELSE oiLimitID = 0.
         END.
         ELSE DO:   
            FOR EACH ttLimits WHERE
                     ttLimits.TMRuleSeq = TMCounter.TMRuleSeq AND
                     ttLimits.ToDate   >= TODAY               AND
                     ttLimits.FromDate <= TODAY               AND
                     ttLimits.LimitAmt <= TMCounter.Amount
                BY ttLimits.LimitAmt DESC:
                oiLimitID = ttLimits.LimitID.
                LEAVE.
            END.
         END.
      END. /* ELSE DO: */
   END.
   
   END CASE.
   
   /* if already handled then counter has the limit id */
   IF TMCounter.LimitID >= oiLimitID THEN DO:

      /* YPR-1425 */
      IF NOT (TMCounter.TMRuleSeq EQ 1 AND
         TMCounter.LimitID EQ 3 AND
         (oiLimitID EQ 1 OR oiLimitID EQ 2)) THEN oiLimitID = 0.
   END.

END PROCEDURE.

PROCEDURE pLimitAction:

   DEF INPUT PARAMETER iiLimitID AS INT  NO-UNDO.   
   
   DEF VAR liRequest  AS INT  NO-UNDO.
   DEF VAR lcResult   AS CHAR NO-UNDO.
   DEF VAR lcParam    AS CHAR NO-UNDO EXTENT 3.
   DEF VAR liCount    AS INT  NO-UNDO.
   DEF VAR lcLimitCLI AS CHAR NO-UNDO.
   DEF VAR i AS INT NO-UNDO. 
   DEF VAR lcSMSTxt AS CHAR NO-UNDO. 
   DEF VAR lcSMSSendRule AS CHAR NO-UNDO. 
   DEF VAR lcSender AS CHAR NO-UNDO. 
   DEF VAR lrBarring  AS ROWID NO-UNDO.
   
   /* get the action (request) that is to be done */
   FIND FIRST ttLimits WHERE
              ttLimits.TMRuleSeq = TMCounter.TMRuleSeq AND
              ttLimits.LimitID   = iiLimitID NO-ERROR.
   IF NOT AVAILABLE ttLimits THEN RETURN "".

   /* subscription already terminated -> no action needed */
   FIND MobSub WHERE MobSub.MsSeq = TMQueue.MsSeq NO-LOCK NO-ERROR.
   IF NOT AVAILABLE MobSub THEN liRequest = -1.
   ELSE
   CASE ttLimits.Action:
   
   /* service change */
   WHEN 1 THEN DO:
      lcParam = "".
      DO liCount = 1 TO MIN(3,NUM-ENTRIES(ttLimits.ActionParam)):
         lcParam[liCount] = ENTRY(liCount,ttLimits.ActionParam).
      END.
      
      liRequest = fServiceRequest(TMCounter.MsSeq,
                                  lcParam[1],         /* ServCom */
                                  INTEGER(lcParam[2]),/* SSStat */
                                  lcParam[3],         /* SSParam */
                                  fMakeTS() + 0.0012, /* act, 2min delay */
                                  "",                 /* salesman */
                                  TRUE,               /* fees */
                                  TRUE,               /* sms */          
                                  "TMQueue",          /* creator */
                                  "5",
                                  0,
                                  FALSE,
                                  OUTPUT lcResult).
   END. 
   
   /* SMS */
   WHEN 30 THEN DO:
   
      DO i = 1 TO NUM-ENTRIES(ttLimits.SMSText):

         lcSMSTxt = ENTRY(i, ttLimits.SMSText).

         lcSMSSendRule = fGetSMSSendRule(
                         lcSMSTxt,
                         TODAY,
                         1,
                         OUTPUT lcSender).

         /* YOT-1778 stop send SMS upsells for this month */
         IF (lcSMSSendRule EQ {&SMS_SENDRULE_OFFICEH_EXCEPT_LAST_DAY} OR
             lcSMSSendRule EQ {&SMS_SENDRULE_24H_EXCEPT_LAST_DAY}) AND
            TODAY EQ fLastDayOfMonth(TODAY) AND
            TIME > 75300 /* 20:55 */ THEN DO:
            FIND CURRENT TMCounter EXCLUSIVE-LOCK.
            ASSIGN TMCounter.LimitID = iiLimitID.
            RETURN "".
         END.

         liRequest = fSMSRequest(TMQueue.MsSeq,
                                 9,                  /* type=info */
                                 "InvText",          /* source of message */
                                 lcSMSTxt,
                                 fSecOffSet(fMakeTS(),120), /* act, 2min delay */
                                 "5",
                                 "TMQueue",
                                 lcSender,
                                 OUTPUT lcResult). 
      END.
   END.
    
   /* barring */
   WHEN 35 THEN DO:
      
      /* ongoing request */
      IF fCheckBarrStatus(TMCounter.MsSeq, OUTPUT lcResult, OUTPUT lrBarring)
         THEN DO:
         /* if pending was done from this same event (for another rule)
            then skip this */    
         IF llBarringDone 
         THEN RETURN "".
         ELSE RETURN "UNDO".
      END.
       
      /* already on */
      ELSE IF fIsInList(ttLimits.ActionParam,lcResult) THEN liRequest = 1.
     
      ELSE DO:

         /* subscription already terminated -> no action needed */
         FIND MobSub WHERE MobSub.MsSeq = TMCounter.MsSeq NO-LOCK NO-ERROR.
         IF NOT AVAILABLE MobSub THEN liRequest = -1.
         
         /* create barring request */   
         ELSE DO: 
      
            /* YPR-1425 */
            IF ((ttLimits.TMRuleSeq EQ liPremiumFraudPostSeq AND
               ttLimits.LimitId EQ 3) OR
               ttLimits.TMRuleSeq EQ liPremiumFraudPreSeq) AND
               lcResult NE "OK" THEN RETURN "".
         
            lcSMSTxt = ttLimits.SMSText.

            IF ttLimits.SMSText > "" THEN DO:
               
               lcSMSSendRule = fGetSMSSendRule(
                               ttLimits.SMSText,
                               TODAY,
                               1,
                               OUTPUT lcSender).
               IF (lcSMSSendRule EQ {&SMS_SENDRULE_OFFICEH_EXCEPT_LAST_DAY} OR
                   lcSMSSendRule EQ {&SMS_SENDRULE_24H_EXCEPT_LAST_DAY}) AND
                   TODAY EQ fLastDayOfMonth(TODAY) AND
                   TIME > 75300 /* 20:55 */ THEN lcSMSTxt = "".
            END.

            RUN barrengine (TMCounter.MsSeq,
                            ttLimits.ActionParam + "=1",
                            "5",                /* source  */
                            "TMQueue",          /* creator */
                            fMakeTS() + 0.0012, /* activate, 2min delay */
                            lcSMSTxt,
                            OUTPUT lcResult).

            /* another barring request was created after last check */
            IF lcResult = "ONC" THEN RETURN "UNDO".
                               
            ASSIGN 
               liRequest  = 0
               lcLimitCLI = MobSub.CLI.
                                
            liRequest = INTEGER(lcResult) NO-ERROR. 
                               
            IF liRequest > 0 THEN DO:
               FIND FIRST MsRequest WHERE MsRequest.MsRequest = liRequest
               EXCLUSIVE-LOCK NO-ERROR.
               IF AVAILABLE MsRequest THEN DO: 
                  ASSIGN
                     MsRequest.ReqIParam3 = ttLimits.TMRuleSeq
                     MsRequest.ReqIParam4 = iiLimitID
                     llBarringDone        = TRUE.
                  RELEASE MsRequest.   
               END.   
            END.

         END.             
         
      END.
   END.

   OTHERWISE liRequest = -1.
    
   END CASE.

   IF liRequest = 0 THEN 
      fErrorLog(TMCounter.CustNum,
                lcLimitCLI,
                "Creation of a request (" + 
                STRING(ttLimits.Action) + 
                ") failed when limit " +
                STRING(TMCounter.TMRuleSeq) + "/" +
                STRING(iiLimitID) + " was exceeded." + CHR(10) +
                lcResult + CHR(10) + 
                "Counter amount: " + STRING(TMCounter.Amount)).

   ELSE IF ttLimits.Action NE 35 THEN DO:
      FIND FIRST MsRequest WHERE MsRequest.MsRequest = liRequest
         EXCLUSIVE-LOCK NO-ERROR.
      IF AVAILABLE MsRequest THEN DO: 
         ASSIGN
            MsRequest.SMSText   = ttLimits.SMSText WHEN ttLimits.Action NE 30
            MsRequest.ReqSource = "5".
         RELEASE MsRequest.   
      END.   
   END.
    
   /* mark to counter that exceeding the limit has been handled */
   IF liRequest NE 0 THEN DO: 
      FIND CURRENT TMCounter EXCLUSIVE-LOCK.
      ASSIGN
         TMCounter.LimitID = IF liRequest > 0 
                             THEN iiLimitID
                             ELSE 99
         liActions         = liActions + 1.
   END.

   RETURN "".
   
END PROCEDURE.

