/* ----------------------------------------------------------------------
  MODULE .......: bundle_first_month_fee.p
  TASK .........: Calculate first month's fixed fee for bundles 
  APPLICATION ..: TMS
  AUTHOR .......: aam
  CREATED ......: 19.10.10
  Version ......: Yoigo
  ---------------------------------------------------------------------- */

DISABLE TRIGGERS FOR LOAD OF FixedFee.

{commali.i}
{cparam2.i}
{timestamp.i}
{tmsconst.i}
{finvnum.i}
{funcrunprocess_update.i}
{bundle_first_month_fee.i}
{fbundle.i}

DEF INPUT  PARAMETER idaFromDate      AS DATE NO-UNDO.
DEF INPUT  PARAMETER idaToDate        AS DATE NO-UNDO.
DEF INPUT  PARAMETER iiInvCust        AS INT  NO-UNDO.
DEF INPUT  PARAMETER iiFRProcessID    AS INT  NO-UNDO.
DEF INPUT  PARAMETER iiUpdateInterval AS INT  NO-UNDO.
DEF INPUT  PARAMETER icRunMode        AS CHAR NO-UNDO.
DEF OUTPUT PARAMETER oiHandled        AS INT  NO-UNDO.

DEF VAR ldPeriodFrom    AS DEC  NO-UNDO.
DEF VAR ldPeriodFromSTC AS DEC  NO-UNDO.
DEF VAR ldPeriodTo      AS DEC  NO-UNDO.
DEF VAR lcIPLContracts  AS CHAR NO-UNDO.
DEF VAR lcBONOContracts AS CHAR NO-UNDO.
DEF VAR liCollected     AS INT  NO-UNDO. 
DEF VAR ldeCollectStart AS DEC NO-UNDO. 
DEF VAR ldeCollectEnd   AS DEC NO-UNDO. 

DEF VAR lcAllowedDSS2SubsType         AS CHAR NO-UNDO.
DEF VAR lcFirstMonthUsageBasedBundles AS CHAR  NO-UNDO.

DEF TEMP-TABLE ttSub NO-UNDO
   FIELD MsSeq        AS INT
   FIELD ServiceLimit AS CHAR
   FIELD FromDate     AS DATE
   FIELD ToDate       AS DATE
   INDEX MsSeq MsSeq.

/******** Main start ********/

ASSIGN
   /* set 1 second after midnight to skip STC contract activations */
   ldPeriodFrom    = fMake2Dt(idaFromDate,1)
   ldPeriodFromSTC = fMake2Dt(idaFromDate,0)
   ldPeriodTo      = fMake2Dt(idaToDate,86399)
   lcIPLContracts  = fCParamC("IPL_CONTRACTS")
   lcBONOContracts = fCParamC("BONO_CONTRACTS")
   lcAllowedDSS2SubsType = fCParamC("DSS2_SUBS_TYPE")
   lcFirstMonthUsageBasedBundles = fCParamC("FIRST_MONTH_USAGE_BASED_BUNDLES")
   ldeCollectStart = fMakeTS().

IF iiInvCust > 0 THEN
   RUN pGetCustomerSubscriptions(iiInvCust).
ELSE
   RUN pGetAllSubscriptions.

IF RETURN-VALUE BEGINS "ERROR" THEN RETURN RETURN-VALUE.

ldeCollectEnd = fMakeTs().

RUN pCalculateFees. 
IF RETURN-VALUE BEGINS "ERROR" THEN RETURN RETURN-VALUE.

RETURN "". 

FINALLY:
   EMPTY TEMP-TABLE ttSub NO-ERROR.
END.

/******** Main end ********/


PROCEDURE pGetCustomerSubscriptions:

   DEF INPUT PARAMETER iiInvCust AS INT  NO-UNDO.

   DEF VAR lcBundleId     AS CHAR NO-UNDO.
   DEF VAR ldaMsReqDate   AS DATE No-UNDO.

   DEF BUFFER bMsRequest FOR MsRequest.

   lcBundleId = fGetDSSId(iiInvCust,ldPeriodTo).
   
   FOR EACH MsOwner NO-LOCK WHERE
            MsOwner.InvCust = iiInvCust
   BREAK BY MsOwner.MsSeq:
   
      IF FIRST-OF(MsOwner.MsSeq) THEN DO:

         /* If this subscription is linked with DSS or DSS2 then */
         /* it will be calculated based on the DSS bundle usage  */
         IF lcBundleId = {&DSS} OR
            (lcBundleId = "DSS2" AND
             LOOKUP(MsOwner.CLIType,lcAllowedDSS2SubsType) > 0) THEN NEXT.

         FOR EACH MsRequest NO-LOCK WHERE
                  MsRequest.MsSeq   = MsOwner.MsSeq AND
                  MsRequest.ReqType = 8 AND
                  LOOKUP(STRING(MsRequest.ReqStat),"2,9") > 0 AND
                  MsRequest.ActStamp >= ldPeriodFrom AND
                  MsRequest.ActStamp <= ldPeriodTo USE-INDEX MsSeq,
             FIRST DayCampaign NO-LOCK WHERE
                   DayCampaign.Brand = gcBrand AND
                   DayCampaign.DCEvent = MsRequest.ReqCParam3 AND
                   LOOKUP(DayCampaign.DCType,{&PERCONTRACT_RATING_PACKAGE}) > 0:
             
             fTS2Date(INPUT  MsRequest.ActStamp,
                      OUTPUT ldaMsReqDate).

             FIND FIRST FMItem NO-LOCK WHERE
                        FMItem.Brand        = gcBrand              AND
                        FMItem.FeeModel     = DayCampaign.FeeModel AND
                        FMItem.FromDate    <= ldaMsReqDate         AND
                        FMItem.ToDate      >= ldaMsReqDate         AND
                        FMItem.FirstMonthBr = 2 NO-ERROR.

             IF NOT AVAILABLE FMItem THEN NEXT.
 
             /* skip first month fee calculation for contracts originating
                from normal stc/btc (not immediate STC/BTC). */
             IF MsRequest.OrigRequest > 0 THEN DO:
                FIND FIRST bMsRequest NO-LOCK WHERE
                           bMsRequest.MsRequest = MsRequest.OrigRequest NO-ERROR.
                IF AVAIL bMsRequest AND
                   bMsRequest.ActStamp = ldPeriodFromSTC AND
                   (bMsRequest.ReqType EQ {&REQTYPE_SUBSCRIPTION_TYPE_CHANGE} OR 
                    bMsRequest.ReqType EQ {&REQTYPE_BUNDLE_CHANGE}) THEN NEXT.
             END. /* IF MsRequest.OrigRequest > 0 THEN DO: */

             RUN pCollectSubscription(MsRequest.MsSeq,
                                      MsRequest.ReqCParam3).
         END. /* FOR EACH MsRequest NO-LOCK WHERE */
      END. /* IF FIRST-OF(MsOwner.MsSeq) THEN DO: */
   END. /* FOR EACH MsOwner NO-LOCK WHERE */
   
   RETURN "". 

END PROCEDURE. 
 
PROCEDURE pGetAllSubscriptions:

   DEF VAR liCount        AS INT  NO-UNDO. 
   DEF VAR liReqStatus    AS INT  NO-UNDO. 
   DEF VAR lcReqStatuses  AS CHAR NO-UNDO INIT "2,9".
   DEF VAR lcBundleId     AS CHAR NO-UNDO.
   DEF VAR ldaMsReqDate   AS DATE No-UNDO.
   
   DEF BUFFER bMsRequest FOR MsRequest.
   
   DO liCount = 1 TO NUM-ENTRIES(lcReqStatuses):

      liReqStatus = INT(ENTRY(liCount, lcReqStatuses)).

      FOR EACH MsRequest NO-LOCK WHERE
               MsRequest.Brand = gcBrand AND
               MsRequest.ReqType = 8     AND
               MsRequest.ReqStat = liReqStatus    AND
               MsRequest.ActStamp >= ldPeriodFrom AND
               MsRequest.ActStamp <= ldPeriodTo,
         FIRST DayCampaign NO-LOCK WHERE
               DayCampaign.Brand = gcBrand AND
               DayCampaign.DCEvent = MsRequest.ReqCParam3 AND
               LOOKUP(DayCampaign.DCType,{&PERCONTRACT_RATING_PACKAGE}) > 0,
         FIRST MsOwner WHERE 
               MsOwner.MsSeq    = MsRequest.MsSeq AND  
               MsOwner.TSEnd   >= ldPeriodFrom    AND
               MsOwner.TsBegin <= ldPeriodTo NO-LOCK:
       
         fTS2Date(INPUT  MsRequest.ActStamp,
                  OUTPUT ldaMsReqDate).

         FIND FIRST FMItem NO-LOCK WHERE
                    FMItem.Brand        = gcBrand              AND
                    FMItem.FeeModel     = DayCampaign.FeeModel AND
                    FMItem.FromDate    <= ldaMsReqDate         AND
                    FMItem.ToDate      >= ldaMsReqDate         AND
                    FMItem.FirstMonthBr = 2 NO-ERROR.
          
         IF NOT AVAILABLE FMItem THEN NEXT. 
       
         /* skip first month fee calculation for contracts originating
            from normal stc/btc (not immediate STC/BTC). */
         IF MsRequest.OrigRequest > 0 THEN DO:   
            FIND FIRST bMsRequest NO-LOCK WHERE
                       bMsRequest.MsRequest = MsRequest.OrigRequest NO-ERROR.
            IF AVAIL bMsRequest AND
               bMsRequest.ActStamp = ldPeriodFromSTC AND
               (bMsRequest.ReqType EQ {&REQTYPE_SUBSCRIPTION_TYPE_CHANGE} OR 
                bMsRequest.ReqType EQ {&REQTYPE_BUNDLE_CHANGE}) THEN NEXT.
         END. /* IF MsRequest.OrigRequest > 0 THEN DO: */

         lcBundleId = fGetDSSId(MsRequest.CustNum,ldPeriodTo).

         /* If this subscription is linked with DSS or DSS2 then */
         /* it will be calculated based on the DSS bundle usage  */

         IF lcBundleId = {&DSS} OR
            (lcBundleId = "DSS2" AND
             LOOKUP(MsOwner.CLIType,lcAllowedDSS2SubsType) > 0) THEN NEXT.

         RUN pCollectSubscription(MsRequest.MsSeq,
                                  MsRequest.ReqCParam3).

      END. /* FOR EACH MsRequest NO-LOCK WHERE */
   END. /* DO liCount = 1 TO NUM-ENTRIES(lcReqStatuses): */
   
   RETURN "". 

END PROCEDURE. 
                               
PROCEDURE pCollectSubscription:

   DEF INPUT PARAMETER iiMsSeq   AS INT  NO-UNDO.
   DEF INPUT PARAMETER icDCEvent AS CHAR NO-UNDO. 
   
   DEF VAR ldaDate       AS DATE NO-UNDO.
   DEF VAR liTime        AS INT  NO-UNDO.
   
   /*YDR-8824 added validfrom AND validto conditions for getting valid record*/
   FOR FIRST ServiceLimit NO-LOCK WHERE 
             ServiceLimit.GroupCode = icDCEvent AND
             ServiceLimit.ValidFrom <= TODAY    AND
             ServiceLimit.ValidTo   >= TODAY,
      /* packages that were activated (and not terminated) on this period */
       FIRST MServiceLimit NO-LOCK WHERE
             MServiceLimit.MsSeq = iiMsSeq AND
             MServiceLimit.SlSeq = ServiceLimit.SlSeq AND
             MServiceLimit.FromTS >= ldPeriodFrom AND
             MServiceLimit.FromTS <= ldPeriodTo AND
             MServiceLimit.EndTS >= ldPeriodTo:

      /* only bundles */
      IF NOT fIsBundle(ServiceLimit.GroupCode) THEN NEXT.    

      /* First month always usage based */
      IF LOOKUP(icDCEvent,lcFirstMonthUsageBasedBundles) > 0 THEN .

      /* If bundle is terminated because of BTC in first month */
      /* then bundle will be charged based on the usage        */
      ELSE IF CAN-FIND(FIRST MSRequest WHERE
                        MSRequest.MsSeq = iiMsSeq AND
                        MsRequest.ReqType = {&REQTYPE_CONTRACT_TERMINATION} AND
                        MsRequest.ActStamp = ldPeriodTo AND
                        MsRequest.ReqCParam3 = icDCEvent AND
                        MsRequest.ReqSource <> {&REQUEST_SOURCE_BTC} AND
          LOOKUP(STRING(MsRequest.ReqStat),"4,99") = 0
                  USE-INDEX MsSeq) OR
         CAN-FIND(FIRST MSRequest WHERE
                        MSRequest.MsSeq = iiMsSeq AND
                        MsRequest.ReqType = {&REQTYPE_CONTRACT_TERMINATION} AND
                        MsRequest.ActStamp = ldPeriodTo AND
                        MsRequest.ReqCParam3 = icDCEvent AND
                        LOOKUP(MsRequest.ReqCParam3,lcBONOContracts) = 0 AND
                        LOOKUP(MsRequest.ReqCParam3,lcIPLContracts)  = 0 AND
                        MsRequest.ReqSource  = {&REQUEST_SOURCE_BTC}
                        USE-INDEX MsSeq)
      THEN NEXT.
      
      CREATE ttSub.
      ASSIGN
         ttSub.MsSeq        = iiMsSeq 
         ttSub.ServiceLimit = ServiceLimit.GroupCode
         liCollected        = liCollected + 1.
         
      fSplitTS(MServiceLimit.FromTS,
               OUTPUT ldaDate,
               OUTPUT liTime).
      ttSub.FromDate = ldaDate.
      
      fSplitTS(MServiceLimit.EndTS,
               OUTPUT ldaDate,
               OUTPUT liTime).
      ttSub.ToDate = ldaDate.

      IF iiUpdateInterval > 0 AND liCollected MOD iiUpdateInterval = 0 THEN DO:
         IF NOT fUpdateFuncRunProgress(iiFRProcessID, -1 * liCollected) THEN
            RETURN "ERROR:Stopped".
      END.   
      
   END.

END PROCEDURE.

PROCEDURE pCalculateFees:

   DEF VAR ldFeeAmount   AS DEC  NO-UNDO.
   DEF VAR lcFeeBillCode AS CHAR NO-UNDO.
   DEF VAR liPeriod      AS INT  NO-UNDO.
   DEF VAR liCount       AS INT  NO-UNDO.
   
   DEF BUFFER bFixedFee FOR FixedFee.
   

   liPeriod = YEAR(idaToDate) * 100 + MONTH(idaToDate).

   FOR EACH ttSub,
      FIRST DayCampaign NO-LOCK WHERE
            DayCampaign.Brand   = gcBrand AND
            DayCampaign.DCEvent = ttSub.ServiceLimit,
      FIRST FixedFee NO-LOCK USE-INDEX HostTable WHERE
            FixedFee.Brand     = gcBrand AND
            FixedFee.HostTable = "MobSub" AND
            FixedFee.KeyValue  = STRING(ttSub.MsSeq) AND
            FixedFee.FeeModel  = DayCampaign.FeeModel AND
            FixedFee.CalcObj   = DayCampaign.DCEvent AND
            FixedFee.InUse     = TRUE AND
            FixedFee.BegDate  >= idaFromDate AND
            FixedFee.BegDate  <= idaToDate AND
            FixedFee.EndPer   >= liPeriod,
      FIRST FMItem NO-LOCK WHERE
            FMItem.Brand     = gcBrand AND
            FMItem.FeeModel  = FixedFee.FeeModel AND
            FMItem.FromDate <= FixedFee.BegDate AND
            FMItem.ToDate   >= FixedFee.BegDate AND
            FMItem.FirstMonthBr = 2:

      /* already billed */
      FIND FIRST FFItem OF FixedFee NO-LOCK NO-ERROR.
      IF NOT AVAILABLE FFItem OR 
         (FFItem.Billed AND icRunMode NE "test") THEN NEXT. 
    
      ldFeeAmount = fCalculateFirstMonthFee(gcBrand,
                                            ttSub.MsSeq,
                                            ttSub.ServiceLimit,
                                            FixedFee.Amt,
                                            liPeriod).

      IF icRunMode EQ "test" THEN oiHandled = oiHandled + 1.
      ELSE DO TRANS:
      
         IF FFItem.Billed THEN NEXT.

         FIND CURRENT FFItem EXCLUSIVE-LOCK.
             
         FFItem.Amt = ldFeeAmount.
             
         FIND FIRST bFixedFee WHERE
                    ROWID(bFixedFee) = ROWID(FixedFee) EXCLUSIVE-LOCK.
         
         ASSIGN 
            /* pmf: partial monthly fee */
            bFixedFee.ServiceLimitGroup = "PMF:" + ttSub.ServiceLimit + ":" +
                                          STRING(ldFeeAmount)
            oiHandled = oiHandled + 1.
      END.

      RELEASE FFItem.
      RELEASE FixedFee.
      
      IF iiUpdateInterval > 0 AND oiHandled MOD iiUpdateInterval = 0 THEN DO:
         IF NOT fUpdateFuncRunProgress(iiFRProcessID,oiHandled) THEN
            RETURN "ERROR:Stopped".
      END.   
    
   END.

   IF oiHandled > 0 AND iiInvCust = 0 THEN DO TRANS:
      CREATE ActionLog.
      ASSIGN 
         ActionLog.Brand        = gcBrand   
         ActionLog.TableName    = "FixedFee"  
         ActionLog.KeyValue     = STRING(YEAR(TODAY),"9999") + 
                                  STRING(MONTH(TODAY),"99")  +
                                  STRING(DAY(TODAY),"99")
         ActionLog.ActionID     = "BUNDLEFEE" + 
                                  (IF icRunMode eq "test" THEN "TEST" ELSE "")
         ActionLog.ActionPeriod = YEAR(TODAY) * 100 + 
                                  MONTH(TODAY)
         ActionLog.ActionDec    = oiHandled
         ActionLog.ActionChar   = "Collection started: " +
                                    fts2hms(ldeCollectStart) + CHR(10) + 
                                  "Collection ended: " + 
                                    fts2hms(ldeCollectEnd) + CHR(10) +
                                  STRING(liCollected) + 
                                    " cases were collected" + CHR(10) +
                                  STRING(oiHandled) + 
                                  " first month fees were updated"
         ActionLog.ActionStatus = 3
         ActionLog.UserCode     = katun
         ActionLog.FromDate     = idaFromDate
         ActionLog.ToDate       = idaToDate.
         ActionLog.ActionTS     = fMakeTS().
   END.

   RETURN "".

END PROCEDURE.


