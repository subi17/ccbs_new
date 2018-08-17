&IF "{&TMQUEUE_ANALYSIS_I}" NE "YES" 
&THEN

&GLOBAL-DEFINE TMQUEUE_ANALYSIS_I YES

{Syst/commali.i}
{Func/cparam2.i}
{Syst/tmsconst.i}
{Func/istc.i}
{Func/bundleupsells.i}
   
DEF VAR lcPMDUBUpsell     AS CHAR NO-UNDO.
DEF VAR lcIPLData         AS CHAR NO-UNDO. 
DEF VAR lcBonoData        AS CHAR NO-UNDO. 
DEF VAR lcBaseContracts   AS CHAR NO-UNDO.
DEF VAR lcDSSUpsell       AS CHAR NO-UNDO.

DEFINE TEMP-TABLE ttServiceLimit NO-UNDO
   FIELD MsSeq     AS INT
   FIELD GroupCode AS CHAR
   INDEX MsSeq MsSeq.


ASSIGN
   lcBaseContracts = fCParamC("TMQueueBaseContract")
   lcPMDUBUpsell   = fCParamC("TMQueuePMDUBUpsell")
   lcDSSUpsell     = fCParamC("TMQueueDSSUpsell")
   lcIPLData       = fCParamC("TMQueueIPLData")
   lcBonoData      = fCParamC("TMQueueBonoData").
      
FUNCTION fGetBDestCount RETURNS DECIMAL (INPUT iiMsSeq       AS INT,
                                     INPUT icBundleId    AS CHAR,
                                     INPUT idActDate     AS DATE):

   DEF VAR liPeriod     AS INT NO-UNDO.

   DEF BUFFER bServiceLimit    FOR ServiceLimit.
   DEF BUFFER bServiceLCounter FOR ServiceLCounter.

   liPeriod = YEAR(idActDate) * 100 + MONTH(idActDate).
   
   /*YDR-2284 added validfrom AND validto conditions for getting valid record*/
   FOR EACH bServiceLimit NO-LOCK WHERE
             bServiceLimit.GroupCode = icBundleId AND
             (bServiceLimit.DialType = 0 OR bServiceLimit.DialType = 4),
       FIRST bServiceLCounter WHERE
             bServiceLCounter.MsSeq  = iiMsSeq AND
             bServiceLCounter.SLSeq  = bServiceLimit.SlSeq AND
             bServiceLCounter.Period = liPeriod NO-LOCK:

      IF bServiceLimit.DialType = 0
      THEN RETURN bServiceLCounter.Amt.
      ELSE RETURN bServiceLCounter.limit.

   END. /* FOR FIRST bServiceLimit NO-LOCK WHERE */

   RETURN 0.

END FUNCTION.

FUNCTION fUpsellBundleCountNew RETURN INT 
   ( INPUT iiTMRuleSeq AS INT, 
     INPUT iiMsSeq AS INT,
     INPUT idtDate AS DATE,
     OUTPUT odeLimitAmt AS DEC):
   
   DEF VAR lcDataBundleContract AS CHAR NO-UNDO. 
   DEF VAR liUpSellCount AS INT NO-UNDO. 
   DEF VAR ldeMonthBegin AS DEC NO-UNDO. 
   DEF VAR ldeMonthEnd AS DEC NO-UNDO. 
   DEF VAR ldaLastDay AS DATE NO-UNDO. 
   DEF VAR ldeTime AS DEC NO-UNDO. 
   DEF VAR ldeUpsellAmt AS DEC NO-UNDO. 
   DEF VAR lcUpsellContracts AS CHAR NO-UNDO.
   DEF VAR lcBONOContracts   AS CHAR NO-UNDO.
   DEF VAR lcIPLContracts    AS CHAR NO-UNDO.
   DEF VAR liLoop            AS INT NO-UNDO. 
   DEF VAR lcBundleUpsell    AS CHAR NO-UNDO.

   DEF BUFFER ServiceLimit   FOR ServiceLimit.
   DEF BUFFER MServiceLimit  FOR MServiceLimit.
   DEF BUFFER MserviceLPool  FOR MServiceLPool.
   DEF BUFFER bMServiceLimit FOR MServiceLimit.
   DEF BUFFER bServiceLimit  FOR ServiceLimit.
   DEF BUFFER bMsRequest     FOR MsRequest.
   DEF BUFFER DayCampaign    FOR DayCampaign.
   
   ASSIGN ldaLastDay  = Func.Common:mLastDayOfMonth(idtDate)
          ldeMonthEnd = Func.Common:mMake2DT(ldaLastDay, 86399)
          ldeTime     = Func.Common:mMake2DT(idtDate, 86399)
          ldeMonthBegin = Func.Common:mMake2DT(DATE(MONTH(idtDate),1,YEAR(idtDate)),0)
          lcIPLContracts  = fCParamC("IPL_CONTRACTS")
          lcBONOContracts = fCParamC("BONO_CONTRACTS").
   
   IF LOOKUP(STRING(iiTMRuleSeq),lcBonoData) > 0 THEN
      lcDataBundleContract = "BONO".
   ELSE IF LOOKUP(STRING(iiTMRuleSeq),lcIPLData) > 0 THEN
      lcDataBundleContract = "CONTRD".
   ELSE IF LOOKUP(STRING(iiTMRuleSeq),lcPMDUBUpsell) > 0 THEN
      lcDataBundleContract = "PMDUB".
   ELSE IF LOOKUP(STRING(iiTMRuleSeq),lcBaseContracts) > 0 THEN DO:
      lcDataBundleContract = "BASE_CONTRACT".

      BONO_CONTRACT_LOOP:
      DO liLoop = 1 TO NUM-ENTRIES(lcBONOContracts):
         FOR EACH ServiceLimit NO-LOCK WHERE 
                  ServiceLimit.GroupCode = ENTRY(liLoop,lcBONOContracts) AND 
                  ServiceLimit.ValidFrom <= idtDate  AND
                  ServiceLimit.ValidTo   >= idtDate,
            FIRST MServiceLimit WHERE 
                  MServiceLimit.MSSeq    = iiMSSeq               AND
                  MServiceLimit.DialType = ServiceLimit.DialType AND
                  MServiceLimit.SlSeq    = ServiceLimit.SlSeq    AND 
                  MServiceLimit.FromTS  <= ldeTime              AND
                  MServiceLimit.EndTS   >= ldeTime NO-LOCK:
            /* Set a high number to prevent limit triggering */
            liUpSellCount = 100.
            LEAVE BONO_CONTRACT_LOOP.
         END.
      END.
   END.

   IF lcDataBundleContract = "" THEN RETURN -1.  /* not a bundle limit */

   FOR EACH bMServiceLimit NO-LOCK WHERE
            bMServiceLimit.MsSeq   = iiMsSeq AND
            bMServiceLimit.EndTS  >= ldeTime, 
      FIRST bServiceLimit NO-LOCK USE-INDEX SlSeq WHERE
            bServiceLimit.SLSeq = bMServiceLimit.SLSeq AND
            bServiceLimit.DialType = {&DIAL_TYPE_GPRS},
      FIRST DayCampaign NO-LOCK WHERE 
            DayCampaign.Brand   = Syst.Var:gcBrand AND
            DayCampaign.DCEvent = bServiceLimit.GroupCode AND
            LOOKUP(STRING(DayCampaign.DCType),
                   {&PERCONTRACT_RATING_PACKAGE}) > 0:
                       
      ASSIGN lcBundleUpsell = fGetDayCampaignUpsells(DayCampaign.DCEvent).
      
      CASE lcDataBundleContract:
         WHEN "BONO" THEN DO:
            IF LOOKUP(DayCampaign.DCEvent,lcBONOContracts) = 0 THEN NEXT.
            lcUpsellContracts = lcBundleUpsell + ",UPGRADE_UPSELL".
         END.
         WHEN "CONTRD" THEN DO:
            IF LOOKUP(DayCampaign.DCEvent,lcIPLContracts) = 0 THEN NEXT.
            lcUpsellContracts = lcBundleUpsell + ",UPGRADE_UPSELL".
         END.
         WHEN "PMDUB" THEN DO:
            IF DayCampaign.DCEvent <> lcDataBundleContract THEN NEXT.
            lcUpsellContracts = lcBundleUpsell.
         END.
         WHEN "BASE_CONTRACT" THEN DO:
            IF NOT (DayCampaign.DCEvent BEGINS "DUB"    OR
                    DayCampaign.DCEvent EQ     "CONT9"  OR
                    DayCampaign.DCEvent EQ     "CONT10" OR
                    DayCampaign.DCEvent EQ     "CONT23" OR
                    DayCampaign.DCEvent EQ     "CONT24" OR
                    DayCampaign.DCEvent EQ     "CONT25" OR
                    DayCampaign.DCEvent EQ     "CONT26" OR
                    DayCampaign.DCEvent EQ     "CONT27" OR
                    DayCampaign.DCEvent BEGINS "CONTS"  OR
                    DayCampaign.DCEvent BEGINS "CONTF"  OR
                    DayCampaign.DCEvent BEGINS "CONTD"  OR
                    DayCampaign.DCevent EQ "CONT15") THEN NEXT.
            lcUpsellContracts = lcBundleUpsell.
         END.
         OTHERWISE NEXT.
      END CASE. /* CASE icBundleType: */

      odeLimitAmt = bMServiceLimit.InclAmt.

      DO liLoop = 1 TO NUM-ENTRIES(lcUpsellContracts):
      FOR EACH ServiceLimit NO-LOCK WHERE 
               ServiceLimit.GroupCode = ENTRY(liLoop,lcUpsellContracts) AND
                ServiceLimit.DialType = {&DIAL_TYPE_GPRS} AND
                ServiceLimit.ValidFrom <= idtDate  AND
                ServiceLimit.ValidTo   >= idtDate,
          FIRST MServiceLimit NO-LOCK WHERE 
                MServiceLimit.MsSeq  = iiMsSeq  AND
                MServiceLimit.DialType = ServiceLimit.DialType AND
                MServiceLimit.SlSeq    = ServiceLimit.SlSeq    AND 
                MServiceLimit.FromTS  <= ldeTime              AND
                MServiceLimit.EndTS   >= ldeTime,
           EACH MServiceLPool NO-LOCK WHERE
                MserviceLPool.MsSeq = MServiceLimit.MsSeq AND
                MserviceLPool.SLSeq = MServiceLimit.SLSeq AND
                MserviceLPool.EndTS <= ldeMonthEnd AND
                MserviceLPool.FromTS >= ldeMonthBegin:

            IF MServiceLimit.Fromts > MserviceLPool.FromTs THEN NEXT.
            
            CASE ServiceLimit.GroupCode:
               WHEN "UPGRADE_UPSELL" THEN ASSIGN
               odeLimitAmt = odeLimitAmt + MserviceLPool.LimitAmt.
               OTHERWISE ASSIGN
                  liUpSellCount = liUpSellCount + 1
                  ldeUpsellAmt = MserviceLPool.LimitAmt WHEN 
                     ldeUpsellAmt < MserviceLPool.LimitAmt.
            END CASE.
      END.
      END.
      
      odeLimitAmt = odeLimitAmt + ldeUpsellAmt.
      RETURN liUpSellCount.
   END.
         
   RETURN -1.

END.

FUNCTION fGetDSSUpsellBundleCount RETURN INT 
    (INPUT iiTMRuleSeq    AS INT, 
     INPUT iiCustNum      AS INT,
     INPUT idtDate        AS DATE,
     OUTPUT odeTotalLimit AS DEC):
   
   DEF VAR liUpSellCount    AS INT  NO-UNDO. 
   DEF VAR ldeMonthBegin    AS DEC  NO-UNDO. 
   DEF VAR ldeMonthEnd      AS DEC  NO-UNDO. 
   DEF VAR ldaLastDay       AS DATE NO-UNDO. 
   DEF VAR ldeToday         AS DEC  NO-UNDO.
   DEF VAR liPeriod         AS INT  NO-UNDO.

   DEF BUFFER ServiceLimit  FOR ServiceLimit.
   DEF BUFFER MserviceLPool FOR MServiceLPool.
   
   ASSIGN ldaLastDay    = Func.Common:mLastDayOfMonth(idtDate)
          ldeMonthEnd   = Func.Common:mMake2DT(ldaLastDay,86399)
          ldeToday      = Func.Common:mMake2DT(idtDate,86399)
          ldeMonthBegin = Func.Common:mMake2DT(DATE(MONTH(idtDate),1,YEAR(idtDate)),0)
          liPeriod      = YEAR(idtDate) * 100 + MONTH(idtDate).

   IF LOOKUP(STRING(iiTMRuleSeq),lcDSSUpsell) = 0 THEN RETURN -1.

   FOR EACH ServiceLimit WHERE
            ServiceLimit.GroupCode BEGINS {&DSS} NO-LOCK,
      FIRST MServiceLimit WHERE
            MServiceLimit.Custnum  = iiCustNum              AND
            MServiceLimit.DialType = ServiceLimit.DialType  AND
            MServiceLimit.SlSeq    = ServiceLimit.SlSeq     AND
            MServiceLimit.FromTS  <= ldeMonthEnd            AND
            MServiceLimit.EndTS   >= ldeMonthBegin NO-LOCK:

      IF LOOKUP(ServiceLimit.GroupCode,{&DSS_BUNDLES}) > 0 THEN
         odeTotalLimit = odeTotalLimit + MServiceLimit.InclAmt.
      ELSE IF ServiceLimit.GroupCode = "DSS_UPSELL_UPGRADE" THEN
         odeTotalLimit = odeTotalLimit + MServiceLimit.InclAmt.
      ELSE DO:
         FOR EACH MServiceLPool WHERE
                  MserviceLPool.CustNum = MServiceLimit.CustNum AND
                  MserviceLPool.SLSeq   = MServiceLimit.SLSeq AND
                  MserviceLPool.EndTS  >= ldeMonthBegin AND
                  MserviceLPool.FromTS <= ldeMonthEnd NO-LOCK:
            liUpSellCount = liUpSellCount + 1.
         END. /* FOR EACH MServiceLPool WHERE */

         FIND FIRST MServiceLPool WHERE
                    MserviceLPool.Custnum = MServiceLimit.Custnum AND
                    MserviceLPool.SLSeq   = MServiceLimit.SLSeq   AND
                    MserviceLPool.EndTS  >= ldeMonthBegin         AND
                    MserviceLPool.FromTS <= ldeMonthEnd NO-LOCK NO-ERROR.
         IF AVAILABLE MserviceLPool THEN
            odeTotalLimit = odeTotalLimit + MserviceLPool.LimitAmt.
      END. /* ELSE DO: */
   END. /* FOR EACH ServiceLimit WHERE */

   RETURN liUpSellCount.

END.

FUNCTION fGetAllDSSUsage RETURN DEC 
    (INPUT iiTMRuleSeq    AS INT, 
     INPUT iiCustNum      AS INT,
     INPUT idtDate        AS DATE):

   DEF VAR liPeriod       AS INT  NO-UNDO.
   DEF VAR ldeTotalUsage  AS DEC  NO-UNDO.

   DEF BUFFER ServiceLimit    FOR ServiceLimit.
   DEF BUFFER ServiceLCounter FOR ServiceLCounter.
   
   liPeriod = YEAR(idtDate) * 100 + MONTH(idtDate).

   IF LOOKUP(STRING(iiTMRuleSeq),lcDSSUpsell) = 0 THEN RETURN 0.

   FOR EACH ServiceLimit WHERE
            ServiceLimit.GroupCode BEGINS {&DSS} NO-LOCK,
      FIRST ServiceLCounter WHERE
            ServiceLCounter.Custnum = iiCustNum            AND
            ServiceLCounter.SlSeq   = ServiceLimit.SlSeq   AND
            ServiceLCounter.Period  = liPeriod NO-LOCK:

      ldeTotalUsage = ldeTotalUsage + ServiceLCounter.amt.
   END. /* FOR EACH ServiceLimit WHERE */

   RETURN ldeTotalUsage.

END.

FUNCTION fGetTotalBundleUsage RETURN LOGICAL
   (INPUT  iiMsSeq       AS INT,
    INPUT  icDCEvent     AS CHAR,
    INPUT  iiCustNum     AS INT,
    INPUT  ldeMonthBegin AS DEC,
    INPUT  ldeMonthEnd   AS DEC,
    OUTPUT ldeTotalLimit AS DEC):

   DEF BUFFER bDayCampaign   FOR DayCampaign.
   DEF BUFFER bServiceLimit  FOR ServiceLimit.
   DEF BUFFER bMServiceLimit FOR MServiceLimit.
   DEF BUFFER bFixedFee      FOR FixedFee.
   DEF BUFFER bSingleFee     FOR SingleFee.
   DEF BUFFER bFMItem        FOR FMItem.
   DEF BUFFER bFFItem        FOR FFItem.

   DEF VAR liContractPeriod AS INT  NO-UNDO. 
   DEF VAR ldaContractDate  AS DATE NO-UNDO.
   DEF VAR liContractTime   AS INT  NO-UNDO.
   DEF VAR llgBundle        AS LOG  NO-UNDO. 
   DEF VAR ldeFirstSecofDay AS DEC  NO-UNDO.
   DEF VAR ldeLastSecofDay  AS DEC  NO-UNDO.

   EMPTY TEMP-TABLE ttServiceLimit.

   ASSIGN 
      liContractTime   = 0
      liContractPeriod = 0
      llgBundle        = FALSE.

   FOR EACH bMServiceLimit NO-LOCK WHERE
            bMServiceLimit.MsSeq   = iiMsSeq       AND
            bMServiceLimit.EndTS  >= ldeMonthBegin AND 
            bMServiceLimit.FromTS <= ldeMonthEnd:
   
      /* Skip DSS contracts */
      IF bMservicelimit.CustNum > 0 THEN NEXT.

      FIND FIRST bServiceLimit NO-LOCK WHERE
                 bServiceLimit.SlSeq    = bMServiceLimit.SlSeq    AND
                 bServiceLimit.dialtype = bMServiceLimit.dialtype NO-ERROR.
      
      IF NOT CAN-FIND(FIRST ttServiceLimit NO-LOCK WHERE 
                            ttServiceLimit.MsSeq     = bMServiceLimit.MsSeq     AND 
                            ttServiceLimit.GroupCode = bServiceLimit.GroupCode) THEN 
      DO:
         CREATE ttServiceLimit.
         ASSIGN 
            ttServiceLimit.MsSeq     = bMServiceLimit.MsSeq 
            ttServiceLimit.GroupCode = bServiceLimit.GroupCode.
      END.
      ELSE NEXT.

      IF icDCEvent NE "" AND 
         bServiceLimit.GroupCode NE icDCEvent THEN NEXT.

      ASSIGN 
         ldeFirstSecofDay = Func.Common:mMake2DT(TODAY,0)
         ldeLastSecofDay  = Func.Common:mMake2DT(TODAY,86399).

      /* pending STC request */
      IF CAN-FIND(FIRST MsRequest NO-LOCK USE-INDEX MsActStamp WHERE
                        MsRequest.MsSeq      = iiMsSeq                             AND
                        MsRequest.ActStamp   = ldeFirstSecofDay                    AND
                       (MsRequest.ReqType    = {&REQTYPE_SUBSCRIPTION_TYPE_CHANGE} OR 
                        MsRequest.ReqType    = {&REQTYPE_BUNDLE_CHANGE})           AND
                        LOOKUP(STRING(MsRequest.ReqStatus),
                                {&REQ_INACTIVE_STATUSES}) = 0)                     THEN
         NEXT. 
      
      /* pending termination request */
      IF CAN-FIND(FIRST MsRequest NO-LOCK WHERE
                        MsRequest.MsSeq      = iiMsSeq                         AND
                        MsRequest.ReqType    = {&REQTYPE_CONTRACT_TERMINATION} AND
                        MsRequest.ReqCParam3 = bServiceLimit.GroupCode         AND
                        LOOKUP(STRING(MsRequest.ReqStatus),
                                {&REQ_INACTIVE_STATUSES}) = 0                  AND
                        MsRequest.ActStamp <= ldeLastSecofDay)                 THEN
         NEXT. 
      
      FIND FIRST bDayCampaign NO-LOCK WHERE 
                 bDayCampaign.Brand   = Syst.Var:gcBrand                 AND 
                 bDayCampaign.DCEvent = bServiceLimit.GroupCode NO-ERROR.
      
      IF NOT AVAIL bDayCampaign THEN NEXT.

      FIND FIRST bFMItem NO-LOCK WHERE  
                 bFMItem.Brand     = Syst.Var:gcBrand               AND       
                 bFMItem.FeeModel  = bDayCampaign.FeeModel AND
                 bFMItem.ToDate   >= TODAY                 AND 
                 bFMItem.FromDate <= TODAY                 NO-ERROR.
     
      IF NOT AVAIL bFMItem THEN NEXT.

      /* Skip Prepaid fee */
      IF bFMItem.BillType EQ "NF" THEN NEXT.
      
      liContractPeriod = YEAR(TODAY) * 100 + MONTH(TODAY).
 
      IF NOT bFMItem.BillMethod THEN DO: 

         /* In case of full months use configuration value for performance
            reasons  */
          IF bMServiceLimit.FromTS < ldeMonthBegin AND
             bMServiceLimit.EndTS > ldeMonthEnd THEN ASSIGN
             ldeTotalLimit = ldeTotalLimit + bFMItem.Amount
             llgBundle     = TRUE.
         ELSE DO:
            FIND FIRST bFixedFee NO-LOCK WHERE
                       bFixedFee.Brand      = Syst.Var:gcBrand                      AND 
                       bFixedFee.HostTable  = "MobSub"                     AND 
                       bFixedFee.CustNum    = iiCustnum                    AND
                       bFixedFee.KeyValue   = STRING(bMServicelimit.MsSeq) AND 
                       bFixedFee.CalcObj    = bServiceLimit.Groupcode      AND
                       bFixedFee.EndPeriod >= liContractPeriod             NO-ERROR.

            IF AVAIL bFixedFee THEN DO:  
               FIND FIRST bFFItem NO-LOCK WHERE 
                          bFFItem.FFNum      = bFixedFee.FFNum  AND 
                          bFFItem.BillPeriod = liContractPeriod NO-ERROR.
          
               IF AVAIL bFFItem THEN 
                  ASSIGN 
                     ldeTotalLimit = ldeTotalLimit + bFFItem.Amt
                     llgBundle     = TRUE.
            END.           
         END.
      END.
      
      IF bFMItem.BillMethod THEN DO:
         FOR EACH bSingleFee NO-LOCK WHERE
                  bSingleFee.Brand      = Syst.Var:gcBrand                      AND
                  bSingleFee.Custnum    = iiCustnum                    AND
                  bSingleFee.HostTable  = "MobSub"                     AND
                  bSingleFee.KeyValue   = STRING(bMServiceLimit.MsSeq) AND
                  bSingleFee.CalcObj    = bServicelimit.SLCode         AND
                  bSingleFee.BillPeriod = liContractPeriod:             
            ASSIGN
               ldeTotalLimit = ldeTotalLimit + bSingleFee.Amt
               llgBundle     = TRUE.

            IF icDCEvent NE "" THEN LEAVE.      
         END.
      END.

   END.

   RETURN llgBundle.

END FUNCTION.

PROCEDURE pUpdateTMCounterLimit:

   DEF INPUT PARAM iiMSSeq AS INT NO-UNDO.
   DEF INPUT PARAM icContract AS CHAR NO-UNDO.
   
   DEF VAR ldaLastDay AS DATE NO-UNDO. 
   DEF VAR ldaFirstDay AS DATE NO-UNDO. 
   DEF VAR ldaToday AS DATE NO-UNDO. 
   DEF VAR liTMRuleSeq AS INT NO-UNDO. 
   DEF VAR ldeTotalDataLimit AS DEC NO-UNDO. 
   DEF VAR liLockLoop AS INT NO-UNDO. 
   DEF VAR liUpSellCount AS INT  NO-UNDO.
   DEF VAR lcBONOContracts AS CHAR NO-UNDO.
   DEF VAR ldaISTC AS DATE NO-UNDO. 

   lcBONOContracts = fCParamC("BONO_CONTRACTS").
   
   IF icContract EQ "CONT9"  OR
      icContract EQ "CONT10" OR
      icContract EQ "CONT15" OR
      icContract EQ "CONT23" OR
      icContract EQ "CONT24" OR
      icContract EQ "CONT25" OR
      icContract EQ "CONT26" OR
      icContract EQ "CONT27" OR
      icContract BEGINS "DUB" OR icContract BEGINS "CONTS" OR
      icContract BEGINS "CONTF" OR icContract EQ "CONTD9" THEN
      liTMRuleSeq = INT(lcBaseContracts) NO-ERROR.
   ELSE IF icContract BEGINS "CONTD" THEN liTMRuleSeq = INT(lcIPLData) NO-ERROR.
   ELSE IF icContract BEGINS "PMDUB" THEN liTMRuleSeq = INT(lcPMDUBUpsell) NO-ERROR.
   ELSE IF LOOKUP(icContract,lcBONOContracts) > 0 THEN liTMRuleSeq = INT(lcBonoData) NO-ERROR.
   ELSE RETURN.

   IF liTMRuleSeq = 0 OR liTMRuleSeq = ? THEN RETURN.
   
   ASSIGN
      ldaToday = TODAY
      ldaFirstDay = DATE(MONTH(ldaToday), 1, YEAR(ldaToday))
      ldaLastDay = Func.Common:mLastDayOfMonth(ldaToday).

   /* have to "disable" possible base contract counter if bono is set */
   IF liTMRuleSeq = INT(lcBonoData) THEN DO:
      FIND FIRST TMCounter EXCLUSIVE-LOCK  WHERE
                 TMCounter.MsSeq = iiMsSeq and
                 TMCounter.TMRuleSeq = INT(lcBaseContracts) AND
                 TMCounter.FromDate >= ldaFirstDay AND
                 TMCounter.ToDate = ldaLastDay NO-WAIT NO-ERROR.
      IF AVAIL TMCounter THEN DO:   
         TMCounter.IntValue = 100.
         RELEASE TMCounter.
      END.
   END. /* IF liTMRuleSeq = INT(lcBonoData) THEN DO: */

   FIND FIRST TMCounter NO-LOCK WHERE
              TMCounter.MsSeq = iiMsSeq and
              TMCounter.TMRuleSeq = liTMRuleSeq AND
              TMCounter.FromDate = ldaFirstDay AND
              TMCounter.ToDate = ldaLastDay NO-ERROR.
   IF NOT AVAIL TMCounter THEN RETURN.

   liUpSellCount = fUpsellBundleCountNew(TMCounter.TMRuleSeq,
                                         iiMsSeq,
                                         TODAY,
                                         OUTPUT ldeTotalDataLimit).

   IF STRING(TMCounter.TMRuleSeq) EQ lcBaseContracts THEN 
      ldaISTC = fGetiSTCDate(iiMsSeq,
                             0,
                             TODAY).


   IF ldaISTC NE ? OR
      (ldeTotalDataLimit > 0 AND
      (TMCounter.LimitAmt < ldeTotalDataLimit OR
       TMCounter.LimitID EQ 0)) THEN DO:

      LOCK_LOOP:
      DO liLockLoop = 1 TO 5:
         
         FIND CURRENT TMCounter EXCLUSIVE-LOCK NO-WAIT NO-ERROR.

         IF LOCKED(TMCounter) THEN DO:
            PAUSE 2 NO-MESSAGE.
            NEXT LOCK_LOOP.
         END.
         
         IF AVAIL TMCounter THEN DO:
            
            IF ldaISTC NE ? THEN DELETE TMCounter.
            ELSE ASSIGN
               TMCounter.IntValue = liUpSellCount
               TMCounter.LimitAmt = ldeTotalDataLimit
               TMCounter.LimitID = 0.
            RELEASE TMCounter.
         END.
         LEAVE.
      END.

   END.
 
END PROCEDURE.

PROCEDURE pUpdateDSSTMCounterLimit:

   DEF INPUT PARAM iiCustNum  AS INT  NO-UNDO.
   
   DEF VAR ldaLastDay         AS DATE NO-UNDO. 
   DEF VAR ldaFirstDay        AS DATE NO-UNDO. 
   DEF VAR ldaToday           AS DATE NO-UNDO. 
   DEF VAR liTMRuleSeq        AS INT  NO-UNDO. 
   DEF VAR ldeTotalDataLimit  AS DEC  NO-UNDO. 
   DEF VAR liLockLoop         AS INT  NO-UNDO. 
   DEF VAR liUpSellCount      AS INT  NO-UNDO.
  
   liTMRuleSeq = INT(lcDSSUpsell) NO-ERROR.

   IF liTMRuleSeq = 0 OR liTMRuleSeq = ? THEN RETURN.
   
   ASSIGN
      ldaToday    = TODAY
      ldaFirstDay = DATE(MONTH(ldaToday),1,YEAR(ldaToday))
      ldaLastDay  = Func.Common:mLastDayOfMonth(ldaToday).

   FIND FIRST TMCounter USE-INDEX Custnum NO-LOCK WHERE
              TMCounter.CustNum   = iiCustNum and
              TMCounter.TMRuleSeq = liTMRuleSeq AND
              TMCounter.FromDate  = ldaFirstDay AND
              TMCounter.ToDate    = ldaLastDay NO-ERROR.
   IF NOT AVAIL TMCounter THEN RETURN.

   liUpSellCount = fGetDSSUpsellBundleCount(INPUT TMCounter.TMRuleSeq,
                                            INPUT iiCustNum,
                                            INPUT ldaToday,
                                            OUTPUT ldeTotalDataLimit).
   IF ldeTotalDataLimit >= 0 AND
      TMCounter.LimitAmt <> ldeTotalDataLimit THEN DO:

      LOCK_LOOP:
      DO liLockLoop = 1 TO 5:
         
         FIND CURRENT TMCounter EXCLUSIVE-LOCK NO-WAIT NO-ERROR.

         IF LOCKED(TMCounter) THEN DO:
            PAUSE 2 NO-MESSAGE.
            NEXT LOCK_LOOP.
         END.
         
         IF AVAIL TMCounter THEN DO:

            IF TMCounter.LimitAmt < ldeTotalDataLimit AND
               TMCounter.Amount < ldeTotalDataLimit THEN
               TMCounter.LimitID = 0.

            ASSIGN
               TMCounter.IntValue = liUpSellCount
               TMCounter.LimitAmt = ldeTotalDataLimit.

            RELEASE TMCounter.
         END.
         LEAVE.
      END.

   END.
 
END PROCEDURE.

PROCEDURE pFraudCounterLimit:

   DEFINE INPUT PARAMETER iiMsSeq   AS INT  NO-UNDO. 
   DEFINE INPUT PARAMETER icDCEvent AS CHAR NO-UNDO. 
   DEFINE INPUT PARAMETER iiCustNum AS INT  NO-UNDO. 

   DEF VAR ldaLastDay    AS DATE    NO-UNDO. 
   DEF VAR ldaFirstDay   AS DATE    NO-UNDO.
   DEF VAR ldeMonthEnd   AS DECIMAL NO-UNDO. 
   DEF VAR ldeMonthBegin AS DECIMAL NO-UNDO.
   DEF VAR ldaToday      AS DATE    NO-UNDO. 
   DEF VAR liFraudSeq    AS INT     NO-UNDO. 
   DEF VAR ldeTotalLimit AS DEC     NO-UNDO. 

   ASSIGN
      ldaToday      = TODAY
      ldaFirstDay   = DATE(MONTH(ldaToday), 1, YEAR(ldaToday))
      ldaLastDay    = Func.Common:mLastDayOfMonth(ldaToday)
      ldeMonthBegin = Func.Common:mMake2DT(ldaFirstDay,0)
      ldeMonthEnd   = Func.Common:mMake2DT(ldaLastDay,86399)
      liFraudSeq    = fCParamI("TMQueueTTFSeq").

   IF liFraudSeq = ? OR liFraudSeq = 0 THEN RETURN.

   FIND FIRST TMCounter EXCLUSIVE-LOCK WHERE
              TMCounter.MsSeq     = iiMsSeq     AND 
              TMCounter.TMRuleSeq = liFraudSeq  AND
              TMCounter.FromDate <= TODAY       AND
              TMCounter.ToDate    = ldaLastDay  NO-ERROR.

   IF NOT AVAIL TMCounter THEN DO:
       fGetTotalBundleUsage (iiMsSeq,
                             "",
                             iiCustNum,
                             ldeMonthBegin,
                             ldeMonthEnd,
                             OUTPUT ldeTotalLimit).
       IF ldeTotalLimit > 0 THEN DO:   
          CREATE TMCounter.
          ASSIGN 
             TMCounter.MsSeq     = iiMsSeq 
             TMCounter.CustNum   = iiCustNum
             TMCounter.TMRuleSeq = liFraudSeq
             TMCounter.FromDate  = ldaFirstDay
             TMCounter.ToDate    = ldaLastDay
             TMCounter.Amount    = ldeTotalLimit
             TMCounter.DecValue  = ldeTotalLimit. 
       END.      
   END.
   ELSE DO:
      fGetTotalBundleUsage(iiMsSeq,
                           icDCEvent,
                           iiCustNum,
                           ldeMonthBegin,
                           ldeMonthEnd,
                           OUTPUT ldeTotalLimit).
      ASSIGN
         TMCounter.Amount = TMCounter.Amount + ldeTotalLimit.

   END.

END PROCEDURE.

&ENDIF
