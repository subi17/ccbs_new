&IF "{&fdss_i}" NE "YES" 
&THEN

&GLOBAL-DEFINE fdss_i YES

{Syst/tmsconst.i}
{Func/cparam2.i}
{Func/coinv.i}

/* used with DSS/DSS2 compatible data bundle terminations (YTS-6383) */
FUNCTION fGetDSSMsSeqLimitTerm RETURNS LOG (INPUT  iiCustNum   AS INT,
                                        INPUT  ideActStamp AS DEC,
                                        INPUT  ideBundleTermStamp AS DEC,
                                        OUTPUT oiDSSMsSeq  AS INT,
                                        OUTPUT odeDSSLimit AS DEC,
                                        OUTPUT ocBundleId  AS CHAR):

   DEF BUFFER ServiceLimit FOR ServiceLimit.
   DEF BUFFER MServiceLimit FOR MServiceLimit.
   DEF BUFFER bMServiceLimit FOR MServiceLimit.

   FOR EACH ServiceLimit NO-LOCK WHERE
            {Func/dss_search.i "ServiceLimit.GroupCode"},
       EACH MServiceLimit NO-LOCK WHERE
            MServiceLimit.CustNum  = iiCustNum             AND
            MServiceLimit.DialType = ServiceLimit.DialType AND
            MServiceLimit.SlSeq    = ServiceLimit.SlSeq    AND
            MServiceLimit.EndTS   >= ideActStamp:

      IF MServiceLimit.FromTs <= ideBundleTermStamp OR
         (MServiceLimit.FromTs EQ TRUNC(MServiceLimit.FromTs,0) AND
          CAN-FIND(FIRST bMServiceLimit NO-LOCK WHERE
                         bMServiceLimit.CustNum  = MServiceLimit.Custnum AND
                         bMServiceLimit.DialType = MServiceLimit.DialType AND
                         bMServiceLimit.SlSeq    = MServiceLimit.SlSeq    AND
                         bMServiceLimit.EndTS    = Func.Common:mSecOffSet(MServiceLimit.FromTs,-1) AND
                         bMServiceLimit.FromTs  <= ideBundleTermStamp)) THEN DO:

         ASSIGN oiDSSMsSeq  = MServiceLimit.MsSeq
                odeDSSLimit = MServiceLimit.InclAmt
                ocBundleId  = ServiceLimit.GroupCode.

         RETURN TRUE.
      END.
   END. /* FOR FIRST ServiceLimit NO-LOCK WHERE */

   RETURN FALSE.
END FUNCTION.

FUNCTION fGetActiveDSSId RETURNS CHAR (INPUT iiCustNum   AS INT,
                                       INPUT ideActStamp AS DEC):

   DEF BUFFER ServiceLimit FOR ServiceLimit.
   DEF BUFFER MServiceLimit FOR MServiceLimit.

   FOR EACH ServiceLimit NO-LOCK WHERE
            {Func/dss_search.i "ServiceLimit.GroupCode"},
      FIRST MServiceLimit NO-LOCK WHERE
            MServiceLimit.CustNum  = iiCustNum             AND
            MServiceLimit.DialType = ServiceLimit.DialType AND
            MServiceLimit.SlSeq    = ServiceLimit.SlSeq    AND
            MServiceLimit.EndTS   >= ideActStamp:

      RETURN ServiceLimit.GroupCode.
   END. /* FOR FIRST ServiceLimit NO-LOCK WHERE */

   RETURN "".
END FUNCTION.

FUNCTION fGetDSSId RETURNS CHAR (
   INPUT iiCustNum   AS INT,
   INPUT ideActStamp AS DEC):

   DEF BUFFER ServiceLimit FOR ServiceLimit.
   DEF BUFFER MServiceLimit FOR MServiceLimit.

   FOR EACH ServiceLimit NO-LOCK WHERE
            {Func/dss_search.i "ServiceLimit.GroupCode"},
      FIRST MServiceLimit NO-LOCK WHERE
            MServiceLimit.CustNum  = iiCustNum             AND
            MServiceLimit.DialType = ServiceLimit.DialType AND
            MServiceLimit.SlSeq    = ServiceLimit.SlSeq    AND
            MServiceLimit.EndTS   >= ideActStamp           AND
            MServiceLimit.FromTs  <= ideActStamp:

      RETURN ServiceLimit.GroupCode.
   END.

   RETURN "".
END FUNCTION.

FUNCTION fGetDSSUsage RETURNS DEC (INPUT iiCustNum    AS INT,
                                   INPUT idActDate    AS DATE,
                                   OUTPUT odeDSSLimit AS DEC):

   DEF BUFFER ServiceLimit FOR ServiceLimit.
   DEF BUFFER MServiceLimit FOR MServiceLimit.
   DEF BUFFER ServiceLCounter FOR ServiceLCounter.   

   DEF VAR liPeriod    AS INT NO-UNDO.
   DEF VAR ldeActStamp AS DEC NO-UNDO.
   DEF VAR ldeAmount   AS DEC NO-UNDO.

   liPeriod = YEAR(idActDate) * 100 + MONTH(idActDate).
   ldeActStamp = Func.Common:mHMS2TS(Func.Common:mLastDayOfMonth(idActDate),"23:59:59").

   FOR EACH ServiceLimit NO-LOCK WHERE
            {Func/dss_search.i "ServiceLimit.GroupCode"},
      FIRST MServiceLimit NO-LOCK WHERE
            MServiceLimit.CustNum = iiCustNum          AND
            MServiceLimit.DialType = ServiceLimit.DialType AND
            MServiceLimit.SlSeq   = ServiceLimit.SlSeq AND
            MServiceLimit.FromTS <= ldeActStamp        AND
            MServiceLimit.EndTS  >= ldeActStamp:

      odeDSSLimit = odeDSSLimit + MServiceLimit.InclAmt.
       
      FIND FIRST ServiceLCounter WHERE
                 ServiceLCounter.CustNum = MServiceLimit.CustNum AND
                 ServiceLCounter.SLSeq   = ServiceLimit.SlSeq    AND
                 ServiceLCounter.Period  = liPeriod NO-LOCK NO-ERROR.
      IF AVAIL ServiceLCounter THEN ldeAmount = ServiceLCounter.Amt.
      ELSE ldeAmount = 0.

      RETURN ldeAmount.
   END. /* FOR FIRST ServiceLimit NO-LOCK WHERE */

   RETURN 0.

END FUNCTION.

FUNCTION fOngoingDSSTerm RETURNS LOG (INPUT iiCustnum   AS INT,
                                      INPUT ideActStamp AS DEC):
   
   DEF VAR llExist AS LOG NO-UNDO.
   
   llExist = CAN-FIND (FIRST MsRequest NO-LOCK WHERE
                       MsRequest.Brand = Syst.Var:gcBrand           AND
                       MsRequest.ReqType = {&REQTYPE_DSS}  AND
                       MsRequest.Custnum = iiCustnum       AND
                       MsRequest.ReqCParam1 = "DELETE"     AND
                       MsRequest.ActStamp  <= ideActStamp  AND
                       LOOKUP(STRING(MsRequest.ReqStatus),
                              {&REQ_INACTIVE_STATUSES}) = 0).

   RETURN llExist.
   
END FUNCTION. /* FUNCTION fOngoingDSSTerm */

FUNCTION fIsDSSActive RETURNS LOG (INPUT iiCustNum    AS INT,
                                   INPUT ideActStamp  AS DEC):

   DEF BUFFER ServiceLimit FOR ServiceLimit.

   FOR EACH ServiceLimit NO-LOCK WHERE
            {Func/dss_search.i "ServiceLimit.GroupCode"}:
       IF CAN-FIND (FIRST MServiceLimit NO-LOCK WHERE
                          MServiceLimit.CustNum = iiCustNum          AND
                          MServiceLimit.DialType = ServiceLimit.DialType AND
                          MServiceLimit.SlSeq   = ServiceLimit.SlSeq AND
                          MServiceLimit.FromTS <= ideActStamp        AND
                          MServiceLimit.EndTS  >= ideActStamp) THEN
       RETURN TRUE.
   END. /* FOR FIRST ServiceLimit NO-LOCK WHERE */

   RETURN FALSE.

END FUNCTION.

FUNCTION fOngoingDSSAct RETURNS LOG (INPUT iiCustnum AS INT):
   
   DEF VAR llExist AS LOG NO-UNDO.
   
   llExist = CAN-FIND (FIRST MsRequest NO-LOCK WHERE
                       MsRequest.Brand = Syst.Var:gcBrand            AND
                       MsRequest.ReqType = {&REQTYPE_DSS}   AND
                       MsRequest.Custnum = iiCustnum        AND
                       MsRequest.ReqCParam1 = "CREATE"      AND
                       LOOKUP(STRING(MsRequest.ReqStatus),
                              {&REQ_INACTIVE_STATUSES}) = 0).

   RETURN llExist.
   
END FUNCTION. /* FUNCTION fOngoingDSSAct */

FUNCTION fGetDSSMsSeqLimit RETURNS LOG (INPUT  iiCustNum   AS INT,
                                        INPUT  ideActStamp AS DEC,
                                        OUTPUT oiDSSMsSeq  AS INT,
                                        OUTPUT odeDSSLimit AS DEC,
                                        OUTPUT ocBundleId  AS CHAR):

   DEF BUFFER Servicelimit FOR Servicelimit.
   DEF BUFFER Mservicelimit FOR mServicelimit.
   FOR EACH ServiceLimit NO-LOCK WHERE
            {Func/dss_search.i "ServiceLimit.GroupCode"},
      FIRST MServiceLimit NO-LOCK WHERE
            MServiceLimit.CustNum  = iiCustNum             AND
            MServiceLimit.DialType = ServiceLimit.DialType AND
            MServiceLimit.SlSeq    = ServiceLimit.SlSeq    AND
            MServiceLimit.EndTS   >= ideActStamp:

      ASSIGN oiDSSMsSeq  = MServiceLimit.MsSeq
             odeDSSLimit = MServiceLimit.InclAmt
             ocBundleId  = ServiceLimit.GroupCode.

      RETURN TRUE.
   END. /* FOR FIRST ServiceLimit NO-LOCK WHERE */

   RETURN FALSE.
END FUNCTION.

FUNCTION fGetTotalDSSUsage RETURNS LOG (INPUT iiCustNum    AS INT,
                                        INPUT idActDate    AS DATE,
                                        OUTPUT ocBundleId  AS CHAR,
                                        OUTPUT odeDSSLimit AS DEC,
                                        OUTPUT odeDSSUsage AS DEC):

   DEF VAR liPeriod    AS INT  NO-UNDO.
   DEF VAR ldeActStamp AS DEC  NO-UNDO.
   DEF VAR ldaFromDate AS DATE NO-UNDO.
   DEF VAR ldeFromTS   AS DEC  NO-UNDO.

   DEF BUFFER bServiceLimit   FOR ServiceLimit.
   DEF BUFFER bMServiceLimit  FOR MServiceLimit.
   DEF BUFFER ServiceLimit    FOR ServiceLimit.
   DEF BUFFER MServiceLimit   FOR MServiceLimit.
   DEF BUFFER MServiceLPool   FOR MServiceLPool.
   DEF BUFFER bServiceLCounter FOR ServiceLCounter.

   ASSIGN liPeriod = YEAR(idActDate) * 100 + MONTH(idActDate)
          ldeActStamp = Func.Common:mHMS2TS(Func.Common:mLastDayOfMonth(idActDate),"23:59:59")
          ldaFromDate = DATE(MONTH(idActDate),1,YEAR(idActDate))
          ldeFromTS   = Func.Common:mHMS2TS(ldaFromDate,"00:00:00").

   FOR EACH ServiceLimit NO-LOCK WHERE
            {Func/dss_search.i "ServiceLimit.GroupCode"},
      FIRST MServiceLimit NO-LOCK WHERE
            MServiceLimit.CustNum  = iiCustNum          AND
            MServiceLimit.DialType = ServiceLimit.DialType AND
            MServiceLimit.SlSeq    = ServiceLimit.SlSeq AND
            MServiceLimit.FromTS  <= ldeActStamp        AND
            MServiceLimit.EndTS   >= ldeActStamp:

      ASSIGN odeDSSLimit = MServiceLimit.InclAmt
             ocBundleId  = ServiceLimit.GroupCode.
       
      FIND FIRST bServiceLCounter WHERE
                 bServiceLCounter.CustNum = MServiceLimit.CustNum AND
                 bServiceLCounter.SLSeq   = ServiceLimit.SlSeq    AND
                 bServiceLCounter.Period  = liPeriod NO-LOCK NO-ERROR.
      IF AVAIL bServiceLCounter THEN
         odeDSSUsage = bServiceLCounter.Amt.

      FOR EACH bServiceLimit NO-LOCK WHERE
               bServiceLimit.GroupCode BEGINS "DSS" AND
              (bServiceLimit.GroupCode = "DSS200_UPSELL" OR
               bServiceLimit.GroupCode MATCHES "DSS*FLEX*UPSELL" OR 
               bServiceLimit.GroupCode = ServiceLimit.Groupcode + "_UPSELL"),
          FIRST bMServiceLimit NO-LOCK WHERE
                bMServiceLimit.CustNum  = iiCustNum          AND
                bMServiceLimit.DialType = bServiceLimit.DialType AND
                bMServiceLimit.SlSeq    = bServiceLimit.SlSeq AND
                bMServiceLimit.FromTS  <= ldeActStamp        AND
                bMServiceLimit.EndTS   >= ldeActStamp:

         FIND FIRST MServiceLPool WHERE
                    MserviceLPool.MsSeq   = bMServiceLimit.MsSeq   AND
                    MserviceLPool.SLSeq   = bMServiceLimit.SLSeq   AND
                    MserviceLPool.FromTS <= ldeActStamp            AND
                    MserviceLPool.EndTS  >= ldeFromTS NO-LOCK NO-ERROR.
         IF AVAILABLE MserviceLPool THEN
            odeDSSLimit = odeDSSLimit + MserviceLPool.LimitAmt.

         FIND FIRST bServiceLCounter WHERE
                    bServiceLCounter.CustNum = bMServiceLimit.CustNum AND
                    bServiceLCounter.SLSeq   = bServiceLimit.SlSeq    AND
                    bServiceLCounter.Period  = liPeriod NO-LOCK NO-ERROR.
         IF AVAIL bServiceLCounter THEN
            odeDSSUsage = odeDSSUsage + bServiceLCounter.Amt.
      END.

      odeDSSUsage = odeDSSUsage / 1024 / 1024.

      RETURN TRUE.
   END. /* FOR EACH ServiceLimit NO-LOCK WHERE */

   RETURN FALSE.

END FUNCTION.

FUNCTION fGetOtherBundleUsages RETURNS DEC (INPUT iiCustNum   AS INT,
                                            INPUT iiPeriod    AS INT):

   DEF VAR ldeConsumedData        AS DEC  NO-UNDO.
   DEF VAR ldFromDate             AS DATE NO-UNDO.
   DEF VAR ldToDate               AS DATE NO-UNDO.
   DEF VAR ldeFromTS              AS DEC  NO-UNDO.
   DEF VAR ldeEndTS               AS DEC  NO-UNDO.
   DEF VAR ldeServiceLCounterAmt  AS DEC  NO-UNDO.
   DEF VAR lcExcludeBundles       AS CHAR NO-UNDO.
   DEF VAR lcAllowedDSS2SubsType  AS CHAR NO-UNDO.
   DEF VAR lcBundleId             AS CHAR NO-UNDO.
  
   DEF BUFFER bMServiceLimit   FOR MServiceLimit.
   DEF BUFFER bServiceLimit    FOR ServiceLimit.
   DEF BUFFER bServiceLCounter FOR ServiceLCounter.
   DEF BUFFER bMsOwner         FOR MsOwner.
   DEF BUFFER bDayCampaign     FOR DayCampaign.

   ASSIGN ldFromDate = fInt2Date(iiPeriod,1)
          ldToDate   = fInt2Date(iiPeriod,2)
          ldeFromTS  = Func.Common:mMake2DT(ldFromDate,0)
          ldeEndTS   = Func.Common:mMake2DT(ldToDate,86399)
          lcExcludeBundles = fCParamC("EXCLUDE_BUNDLES").

   lcBundleId = fGetActiveDSSId(iiCustNum,ldeEndTS).
   IF lcBundleId = "DSS2" THEN
      lcAllowedDSS2SubsType = fCParamC("DSS2_SUBS_TYPE").

   FOR EACH bMsOwner NO-LOCK WHERE
            bMsOwner.InvCust  = iiCustnum  AND
            bMsOwner.TsBegin <= ldeEndTS   AND
            bMsOwner.TsEnd   >= ldeFromTS  AND
            NOT bMsOwner.PayType
      BREAK BY bMsOwner.MsSeq:
      IF FIRST-OF(bMsOwner.MsSeq) THEN DO:

         IF lcBundleId = "DSS2" AND
            LOOKUP(bMsOwner.CLIType,lcAllowedDSS2SubsType) = 0 THEN NEXT.

         FOR EACH bMServiceLimit WHERE
                  bMServiceLimit.MsSeq   = bMsOwner.MsSeq AND
                  bMServiceLimit.DialType = {&DIAL_TYPE_GPRS} AND
                  bMServiceLimit.FromTS <= ldeEndTS      AND
                  bMServiceLimit.EndTS  >= ldeEndTS  NO-LOCK,
            FIRST bServiceLimit NO-LOCK WHERE
                  bServiceLimit.SLSeq = bMServiceLimit.SLSeq,
            FIRST bDayCampaign WHERE
                  bDayCampaign.Brand = Syst.Var:gcBrand AND
                  bDayCampaign.DCEvent = bServiceLimit.GroupCode NO-LOCK:

            IF LOOKUP(STRING(bDayCampaign.DCType),
                      {&PERCONTRACT_RATING_PACKAGE}) = 0 AND
               bDayCampaign.DCType NE {&DCTYPE_POOL_RATING} THEN NEXT.

            IF bDayCampaign.DCEvent BEGINS {&DSS} OR
               LOOKUP(bDayCampaign.DCEvent,lcExcludeBundles) > 0 THEN NEXT.

            FIND FIRST bServiceLCounter WHERE
                       bServiceLCounter.MSSeq  = bMsOwner.MsSeq      AND
                       bServiceLCounter.SLSeq  = bServiceLimit.SlSeq AND
                       bServiceLCounter.Period = iiPeriod NO-LOCK NO-ERROR.
            IF AVAILABLE bServiceLCounter THEN DO:
               ldeServiceLCounterAmt = (bServiceLCounter.Amt / 1024 / 1024).
               IF bMServiceLimit.InclAmt < ldeServiceLCounterAmt THEN
                  ldeConsumedData = ldeConsumedData + bMServiceLimit.InclAmt.
               ELSE
                  ldeConsumedData = ldeConsumedData + ldeServiceLCounterAmt.
            END. /* IF AVAILABLE bServiceLCounter THEN DO: */
         END. /* FOR EACH bMServiceLimit WHERE */
      END. /* IF FIRST-OF(bMsOwner.MsSeq) THEN DO: */
   END. /* FOR EACH bMsOwner NO-LOCK WHERE */

   RETURN ldeConsumedData.

END FUNCTION.


&ENDIF
