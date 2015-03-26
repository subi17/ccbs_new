&IF "{&TMQUEUE_ANALYSIS_I}" NE "YES" 
&THEN

&GLOBAL-DEFINE TMQUEUE_ANALYSIS_I YES

{commali.i}
{date.i}
{cparam2.i}
{tmsconst.i}
{istc.i}
   
DEF VAR lcPMDUBUpsell     AS CHAR NO-UNDO.
DEF VAR lcIPLData         AS CHAR NO-UNDO. 
DEF VAR lcBonoData        AS CHAR NO-UNDO. 
DEF VAR lcBaseContracts   AS CHAR NO-UNDO.
DEF VAR lcDSSUpsell       AS CHAR NO-UNDO.

ASSIGN
   lcBaseContracts = fCParamC("TMQueueBaseContract")
   lcPMDUBUpsell   = fCParamC("TMQueuePMDUBUpsell")
   lcDSSUpsell     = fCParamC("TMQueueDSSUpsell")
   lcIPLData       = fCParamC("TMQueueIPLData")
   lcBonoData      = fCParamC("TMQueueBonoData").

FUNCTION fGetBDestCount RETURNS INT (INPUT iiMsSeq       AS INT,
                                     INPUT icBundleId    AS CHAR,
                                     INPUT idActDate     AS DATE):

   DEF VAR liPeriod     AS INT NO-UNDO.
   DEF VAR liDialType   AS INT NO-UNDO.
   DEF VAR liBDestCount AS INT NO-UNDO.

   DEF BUFFER bServiceLimit    FOR ServiceLimit.
   DEF BUFFER bServiceLCounter FOR ServiceLCounter.

   liPeriod = YEAR(idActDate) * 100 + MONTH(idActDate).

   IF icBundleId BEGINS "CONTF" OR
      icBundleId EQ "VOICE100" THEN liDialType = 4.
   ELSE liDialType = 0.

   FOR FIRST bServiceLimit NO-LOCK WHERE
             bServiceLimit.GroupCode = icBundleId AND
             bServiceLimit.DialType  = liDialType,
       FIRST bServiceLCounter WHERE
             bServiceLCounter.MsSeq  = iiMsSeq AND
             bServiceLCounter.SLSeq  = bServiceLimit.SlSeq AND
             bServiceLCounter.Period = liPeriod NO-LOCK:

      IF liDialType = 0 THEN liBDestCount = bServiceLCounter.Amt.
      ELSE liBDestCount = bServiceLCounter.limit.

      RETURN liBDestCount.
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
   DEF VAR lcFLATContracts   AS CHAR NO-UNDO.

   DEF BUFFER ServiceLimit   FOR ServiceLimit.
   DEF BUFFER MServiceLimit  FOR MServiceLimit.
   DEF BUFFER MserviceLPool  FOR MServiceLPool.
   DEF BUFFER bMServiceLimit FOR MServiceLimit.
   DEF BUFFER bServiceLimit  FOR ServiceLimit.
   DEF BUFFER bMsRequest     FOR MsRequest.
   DEF BUFFER DayCampaign    FOR DayCampaign.
   
   ASSIGN ldaLastDay  = fLastDayOfMonth(idtDate)
          ldeMonthEnd = fMake2Dt(ldaLastDay, 86399)
          ldeTime     = fMake2Dt(idtDate, 86399)
          ldeMonthBegin = fMake2Dt(DATE(MONTH(idtDate),1,YEAR(idtDate)),0)
          lcIPLContracts  = fCParamC("IPL_CONTRACTS")
          lcBONOContracts = fCParamC("BONO_CONTRACTS")
          lcFLATContracts = fCParamC("FLAT_CONTRACTS").
   
   IF LOOKUP(STRING(iiTMRuleSeq),lcBonoData) > 0 THEN
      lcDataBundleContract = "BONO".
   ELSE IF LOOKUP(STRING(iiTMRuleSeq),lcIPLData) > 0 THEN
      lcDataBundleContract = "CONTRD".
   ELSE IF LOOKUP(STRING(iiTMRuleSeq),lcPMDUBUpsell) > 0 THEN
      lcDataBundleContract = "PMDUB".
   ELSE IF LOOKUP(STRING(iiTMRuleSeq),lcBaseContracts) > 0 THEN DO:
      lcDataBundleContract = "BASE_CONTRACT".

      FOR EACH ServiceLimit NO-LOCK WHERE 
               LOOKUP(ServiceLimit.GroupCode,lcBONOContracts) > 0 AND 
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
            DayCampaign.Brand   = gcBrand AND
            DayCampaign.DCEvent = bServiceLimit.GroupCode AND
            LOOKUP(STRING(DayCampaign.DCType),
                   {&PERCONTRACT_RATING_PACKAGE}) > 0:
      
      CASE lcDataBundleContract:
         WHEN "BONO" THEN DO:
            IF LOOKUP(DayCampaign.DCEvent,lcBONOContracts) = 0 THEN NEXT.
            lcUpsellContracts = DayCampaign.BundleUpsell + ",UPGRADE_UPSELL".
         END.
         WHEN "CONTRD" THEN DO:
            IF LOOKUP(DayCampaign.DCEvent,lcIPLContracts) = 0 THEN NEXT.
            lcUpsellContracts = DayCampaign.BundleUpsell + ",UPGRADE_UPSELL".
         END.
         WHEN "PMDUB" THEN DO:
            IF DayCampaign.DCEvent <> lcDataBundleContract THEN NEXT.
            lcUpsellContracts = DayCampaign.BundleUpsell.
         END.
         WHEN "BASE_CONTRACT" THEN DO:
            IF NOT (DayCampaign.DCEvent BEGINS "DUB"    OR
                    DayCampaign.DCEvent EQ     "CONT9"  OR
                    DayCampaign.DCEvent EQ     "CONT24" OR
                    DayCampaign.DCEvent BEGINS "CONTS"  OR
                    DayCampaign.DCEvent BEGINS "CONTF"  OR
                    DayCampaign.DCEvent BEGINS "CONTD"  OR
                    DayCampaign.DCevent EQ "CONT15") THEN NEXT.
            lcUpsellContracts = DayCampaign.BundleUpsell.
         END.
         OTHERWISE NEXT.
      END CASE. /* CASE icBundleType: */

      odeLimitAmt = bMServiceLimit.InclAmt.

      FOR EACH ServiceLimit NO-LOCK WHERE 
         LOOKUP(ServiceLimit.GroupCode,lcUpsellContracts) > 0 AND
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
   
   ASSIGN ldaLastDay    = fLastDayOfMonth(idtDate)
          ldeMonthEnd   = fMake2Dt(ldaLastDay,86399)
          ldeToday      = fMake2Dt(idtDate,86399)
          ldeMonthBegin = fMake2Dt(DATE(MONTH(idtDate),1,YEAR(idtDate)),0)
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
   
   IF icContract EQ "CONT9" OR
      icContract EQ "CONT15" OR
      icContract EQ "CONT24" OR
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
      ldaLastDay = fLastDayOfMOnth(ldaToday).

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
      ldaLastDay  = fLastDayOfMOnth(ldaToday).

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

&ENDIF
