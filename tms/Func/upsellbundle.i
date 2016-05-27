
/* 
  upsellbundle.i 
  create  upsell bundle   
*/
&IF "{&upsellbundle}" NE "YES"
&THEN
       
&GLOBAL-DEFINE upsellbundle YES

{Func/timestamp.i}
{Func/fmakemsreq.i}
{Func/date.i}
{Syst/tmsconst.i}
{Mm/fbundle.i}
{Func/fsendsms.i}
{Func/fdss.i}
{Func/fprepaidfee.i}

FUNCTION fGetUpSellBasicContract RETURNS CHAR
   (INPUT iiMsSeq       AS INT,
    INPUT iiCustNum     AS INT,
    INPUT ilPayType     AS LOG,
    INPUT icUpsellType  AS CHAR,
    INPUT icUpsellId    AS CHAR,
    INPUT icCaller      AS CHAR):

   DEF VAR ldTS                  AS DEC  NO-UNDO.
   DEF VAR lcAllowedDSS2SubsType AS CHAR NO-UNDO.

   DEF BUFFER bMServiceLimit FOR MServiceLimit.
   DEF BUFFER bServiceLimit  FOR ServiceLimit.
   DEF BUFFER bMobSub       FOR MobSub. 
   DEF BUFFER DayCampaign   FOR DayCampaign.

   ldTS = fMakeTS().

   /* If caller is bob tool and DSS return basic bundle as DSS. */
   /* If Subs. is postpaid and DSS is active then return Basic bundle as DSS */
   /* Because we can not buy any other upsells when DSS is active            */

   IF icUpsellType = "Customer" THEN DO:
      
      lcAllowedDSS2SubsType = fCParamC("DSS2_SUBS_TYPE").
      
      FIND FIRST bMobSub WHERE 
                 bMobSub.MsSeq = iiMsSeq NO-LOCK NO-ERROR.
      IF NOT AVAIL bMobSub THEN RETURN "".
  
      FOR EACH bServiceLimit NO-LOCK WHERE
          {Func/dss_search.i "bServiceLimit.GroupCode"},
          EACH bMServiceLimit NO-LOCK WHERE
               bMServiceLimit.CustNum  = iiCustNum              AND
               bMServiceLimit.DialType = bServiceLimit.DialType AND
               bMServiceLimit.SlSeq    = bServiceLimit.SlSeq    AND
               bMServiceLimit.FromTS  <= ldTS                   AND
               bMServiceLimit.EndTS   >= ldTS:

         IF NOT ilPayType THEN DO:
            IF bServiceLimit.GroupCode = {&DSS} OR
               (bServiceLimit.GroupCode = "DSS2" AND
                LOOKUP(bMobSub.CLIType,lcAllowedDSS2SubsType) > 0) THEN
               RETURN bServiceLimit.GroupCode.
         END.

         RETURN "".
      END. /* FOR EACH bServiceLimit WHERE */
   END.
   /*Mobsub*/
   ELSE DO:
      IF icCaller EQ {&REQUEST_SOURCE_YOIGO_TOOL} OR
         icUpsellId EQ "DATA200_UPSELL" OR 
         icUpsellId EQ "DSS200_UPSELL"  THEN DO:
         FOR EACH bMServiceLimit NO-LOCK WHERE
                  bMServiceLimit.MsSeq   = iiMsSeq AND
                  bMServiceLimit.DialType = {&DIAL_TYPE_GPRS} AND
                  bMServiceLimit.FromTS <= ldTS    AND
                  bMServiceLimit.EndTS  >= ldTS,
            FIRST bServiceLimit NO-LOCK USE-INDEX SLSeq WHERE
                  bServiceLimit.SLSeq = bMServiceLimit.SLSeq,
            FIRST DayCampaign NO-LOCK WHERE
                  DayCampaign.Brand = gcBrand AND
                  DayCampaign.DCEvent = bServiceLimit.GroupCode AND
                  INDEX(DayCampaign.DCEvent,"UPSELL") = 0 AND
                  DayCampaign.DCEvent <> "BONO_VOIP" AND
                  DayCampaign.DCEvent <> "HSPA_ROAM_EU":
            RETURN bServiceLimit.GroupCode.
         END. /* FOR EACH bMServiceLimit NO-LOCK WHERE */
      END.
      ELSE DO:
         FOR EACH bMServiceLimit NO-LOCK WHERE
                  bMServiceLimit.MsSeq   = iiMsSeq AND
                  bMServiceLimit.DialType = {&DIAL_TYPE_GPRS} AND
                  bMServiceLimit.FromTS <= ldTS    AND
                  bMServiceLimit.EndTS  >= ldTS, 
            FIRST bServiceLimit NO-LOCK USE-INDEX SLSeq WHERE
                  bServiceLimit.SLSeq = bMServiceLimit.SLSeq,
            FIRST DayCampaign NO-LOCK WHERE
                  DayCampaign.Brand = gcBrand AND
                  DayCampaign.DCEvent = bServiceLimit.GroupCode AND
                  DayCampaign.BundleUpsell NE "DATA200_UPSELL" AND
                  DayCampaign.BundleUpsell > "":
            IF {Func/dss_search.i "DayCampaign.DCEvent"} THEN NEXT.
            RETURN bServiceLimit.GroupCode.
         END. /* FOR EACH bMServiceLimit NO-LOCK WHERE */
      END.
   END.

   RETURN "".
END FUNCTION. /* FUNCTION fGetUpSellBasicContract */


FUNCTION fGetUpSellCount RETURNS INT
   (INPUT icDCEvent AS CHAR,
    INPUT iiMsSeq AS INT,
    INPUT iiCustnum AS INT,
    OUTPUT ocError AS CHAR):
   
   DEF VAR liUpSellCount AS INT NO-UNDO. 
   DEF VAR ldeMonthBegin AS DEC NO-UNDO.
   DEF VAR ldeMonthEnd   AS DEC NO-UNDO.
   DEF VAR ldTS          AS DEC NO-UNDO.

   DEF BUFFER bServiceLimit  FOR ServiceLimit.
   DEF BUFFER bMServiceLimit FOR MServiceLimit.
   DEF BUFFER DayCampaign    FOR DayCampaign.

   ASSIGN ldeMonthBegin = fHMS2TS(DATE(MONTH(TODAY),1,YEAR(TODAY)),"00:00:00")
          ldeMonthEnd   = fHMS2TS(fLastDayOfMonth(TODAY),"23:59:59")
          ldTS          = fMakeTS().

   FOR FIRST DayCampaign NO-LOCK WHERE
             DayCampaign.Brand = gcBrand AND
             DayCampaign.DCEvent = icDCEvent,
       FIRST ServiceLimit NO-LOCK WHERE 
             ServiceLimit.GroupCode  = DayCampaign.DCEvent AND 
             ServiceLimit.ValidFrom <= TODAY  AND 
             ServiceLimit.ValidTo   >= TODAY:

        IF icDCEvent BEGINS "DSS" THEN DO:

           FOR EACH MServiceLPool WHERE
                    MServiceLPool.Custnum = iiCustnum AND
                    MServiceLPool.SLSeq   = ServiceLimit.SLSeq AND
                    MServiceLPool.EndTS  <= ldeMonthEnd AND
                    MServiceLPool.FromTS >= ldeMonthBegin NO-LOCK:

               liUpSellCount = liUpSellCount + 1.
               /* don't change error text (ext.upsell_bundles_get) */
               IF liUpSellCount >= DayCampaign.InstanceLimit THEN DO:
                  ocError = SUBST("Upsell limit exceeded",
                                   DayCampaign.InstanceLimit).
                  LEAVE.
               END.
           END. /* FOR EACH MServiceLPool WHERE */
        END.
        ELSE IF icDCEvent = "HSPA_ROAM_EU" OR icDCEvent = {&TARJ_UPSELL} THEN DO:
           FOR EACH MServiceLimit WHERE 
                    MServiceLimit.MSSeq    = iiMsSeq AND
                    MServiceLimit.DialType = ServiceLimit.DialType AND
                    MServiceLimit.SlSeq    = ServiceLimit.SlSeq AND 
                    MServiceLimit.EndTS <= ldeMonthEnd AND
                    MServiceLimit.EndTS > ldeMonthBegin AND
                    MServiceLimit.FromTs >= ldeMonthBegin:
              
              /* YTS-7114 */
              IF icDCEvent EQ "HSPA_ROAM_EU" AND
                 INT(MServiceLimit.FromTS) EQ MServiceLimit.FromTS THEN NEXT.

              liUpSellCount = liUpSellCount + 1.
           END.
        END.
        ELSE IF icDCEvent = "TARJ7_UPSELL" THEN DO:
           FOR FIRST bMServiceLimit WHERE
                     bMServiceLimit.MSSeq    = iiMsSeq AND
                     bMServiceLimit.DialType = ServiceLimit.DialType AND
                     bMServiceLimit.SlSeq    = ServiceLimit.SlSeq AND 
                     bMServiceLimit.EndTS   >= ldTS AND
                     bMServiceLimit.FromTS  <= ldTS NO-LOCK:

              FOR EACH MServiceLPool WHERE 
                       MServiceLPool.MSSeq   = iiMsSeq AND
                       MServiceLPool.SlSeq   = ServiceLimit.SlSeq AND 
                       MServiceLPool.EndTS  <= bMServiceLimit.EndTS AND
                       MServiceLPool.FromTs >= bMServiceLimit.FromTS NO-LOCK:
                 liUpSellCount = liUpSellCount + 1.
              END.
           END.
        END.
        ELSE
           FOR EACH MServiceLPool WHERE
                    MServiceLPool.MsSeq = iiMsSeq AND
                    MServiceLPool.SLSeq = ServiceLimit.SLSeq AND
                    MServiceLPool.EndTS <= ldeMonthEnd AND
                    MServiceLPool.FromTS >= ldeMonthBegin NO-LOCK:

               liUpSellCount = liUpSellCount + 1.
               /* don't change error text (ext.upsell_bundles_get) */
               IF liUpSellCount >= DayCampaign.InstanceLimit THEN DO:
                  ocError = SUBST("Upsell limit exceeded",
                                   DayCampaign.InstanceLimit).
                  LEAVE.
               END.
           END. /* FOR EACH MServiceLPool WHERE */
   END.
   
   RETURN liUpSellCount.

END FUNCTION.

FUNCTION fCreateUpsell RETURNS INT 
   (INPUT iiMsSeq AS INT,
    INPUT icDCEvent AS CHAR,
    INPUT icSource AS CHAR,
    INPUT ideActStamp AS DEC,
    OUTPUT ocError AS CHAR):

   DEF VAR liRequest AS INTEGER NO-UNDO.
   
   liRequest = fPCActionRequest(iiMsSeq,
                                icDCEvent,
                                "act",
                                ideActStamp,
                                TRUE, /* create fee */
                                icSource,
                                "", /* creator */
                                0,
                                FALSE,
                                "",
                                0,
                                0,
                                OUTPUT ocError).
   
   RETURN liRequest.

END FUNCTION.

FUNCTION fCreateUpSellBundle RETURN LOGICAL
   (INPUT iiMsSeq AS INT,
    INPUT icDCEvent AS CHAR,
    INPUT icSource AS CHAR,
    INPUT ideActStamp AS DEC,
    OUTPUT oiRequest AS INT,
    OUTPUT ocError AS CHAR):

   DEF VAR liUpsellCount           AS INT  NO-UNDO. 
   DEF VAR lcBaseContract          AS CHAR NO-UNDO.
   DEF VAR llResult                AS LOG  NO-UNDO.
   DEF VAR ldeBundleFee            AS DEC  NO-UNDO.
   DEF VAR liDSSMsSeq              AS INT  NO-UNDO.
   DEF VAR ldeDSSLimit             AS DEC  NO-UNDO.
   DEF VAR lcDSSBundleId           AS CHAR NO-UNDO.
   DEF VAR lcSMSText               AS CHAR NO-UNDO.

   DEF BUFFER lbMobSub             FOR MobSub. 
   DEF BUFFER bDSSMobSub           FOR MobSub.

   FIND FIRST lbMobSub WHERE 
              lbMobSub.MsSeq = iiMsSeq NO-LOCK NO-ERROR. 
   IF NOT AVAIL lbMobSub THEN DO:
      ocError = "MobSub not found".
      RETURN FALSE.
   END.

   IF NOT CAN-FIND (FIRST DayCampaign WHERE
                          DayCampaign.Brand   = gcBrand   AND
                          DayCampaign.DCEvent = icDCEvent AND
                          DayCampaign.ValidTo >= TODAY NO-LOCK) THEN DO:
      ocError = "Incorrect upsell type".
      RETURN FALSE.
   END.

   /* check if customer level basic contract exist */
   lcBaseContract = fGetUpSellBasicContract(lbMobSub.MsSeq,
                                            lbMobSub.CustNum,
                                            lbMobSub.PayType,
                                            "Customer",
                                            icDCEvent,
                                            icSource).

   /* check if subscription level basic contract exist */
   IF lcBaseContract = "" THEN
      lcBaseContract = fGetUpSellBasicContract(lbMobSub.MsSeq,
                                               lbMobSub.CustNum,
                                               lbMobSub.PayType,
                                               "MobSub",
                                               icDCEvent,
                                               icSource).

   IF lcBaseContract = "" THEN DO:
      ocError = "Data contract does not exist". 
      RETURN FALSE.
   END.
   FIND FIRST DayCampaign WHERE
              DayCampaign.Brand    = gcBrand AND
              DayCampaign.DCEvent  = lcBaseContract AND
              DayCampaign.ValidTo >= TODAY NO-LOCK NO-ERROR.
   IF NOT AVAIL DayCampaign THEN DO:
      ocError = "Incorrect data contract".
      RETURN FALSE.
   END.

   /* Should not allow to create other data upsell once DSS1/2 is active */
   IF LOOKUP(icDCEvent, DayCampaign.BundleUpsell) EQ 0 THEN DO : 
      IF LOOKUP(DayCampaign.DCEvent,{&DSS_BUNDLES}) > 0 THEN
          ocError = icDCEvent + " is not allowed because DSS " +
                    "is active for this customer".
      /* allow upsell to any data contract by bob tool */
      ELSE IF icSource NE {&REQUEST_SOURCE_YOIGO_TOOL} THEN
         ocError = "Incorrect upsell type".
      ELSE IF ocError <> "" THEN
         RETURN FALSE.
   END. /* IF lcCustBaseContract = {&DSS} AND */
  
   /* check for ongoing bundle termination */
   IF LOOKUP(DayCampaign.DCEvent,{&DSS_BUNDLES}) > 0 THEN DO:
      IF fOngoingDSSTerm(lbMobSub.Custnum, ideActStamp) THEN DO:
         ocError = "Data contract does not exist". 
         RETURN FALSE.
      END.
      ELSE DO:
         IF NOT fGetDSSMsSeqLimit(lbMobSub.Custnum,ideActStamp,
                                  OUTPUT liDSSMsSeq,OUTPUT ldeDSSLimit,
                                  OUTPUT lcDSSBundleId)
         THEN DO:
            ocError = "Data contract does not exist".
            RETURN FALSE.
         END.
         ELSE DO:
            FIND FIRST bDSSMobSub WHERE
                       bDSSMobSub.MsSeq = liDSSMsSeq NO-LOCK NO-ERROR.
            IF NOT AVAIL bDSSMobSub THEN DO:
               ocError = "DSS main subscription is already terminated".
               RETURN FALSE.
            END.
         END.
      END. /* ELSE DO: */
   END.

   /* get amount of UpSell bundles */
   liUpsellCount = fGetUpSellCount(INPUT icDCEvent,INPUT iiMsSeq,
                                   INPUT lbMobSub.Custnum,OUTPUT ocError).
   IF ocError > "" THEN RETURN FALSE.

   /* Validate Prepaid Balance before making PMDUB UPSELL activation request */
   IF LOOKUP(icDCEvent,"PMDUB_UPSELL,TARJ7_UPSELL") > 0 THEN DO:
      ldeBundleFee = fgetPrepaidFeeAmount(icDCEvent, TODAY).
      IF icDCEvent = "PMDUB_UPSELL" THEN
                lcSMSText = "PMDUBUBalChk".
      ELSE
                lcSMSText = "UpsellTARJ7NoBal".

      RUN pEnoughBalance(INPUT lbMobSub.CLI,
                         INPUT ldeBundleFee,
                         OUTPUT llResult).
      IF NOT llResult THEN DO:
         ocError = "Not enough balance".
         IF icSource EQ {&REQUEST_SOURCE_EXTERNAL_API} THEN
            RUN pSendSMS(INPUT lbMobSub.MsSeq,
                         INPUT 0,
                         INPUT lcSMSText,
                         INPUT 10,
                         INPUT {&UPSELL_SMS_SENDER},
                         INPUT "").
         RETURN FALSE.
      END.
   END. /* IF LOOKUP(icDCEvent,"PMDUB_UPSELL,TARJ7_UPSELL") > 0 THEN DO: */

   create_upsell:
   DO TRANS:
      
      /* periodical contract request */
      oiRequest = fCreateUpsell(iiMsSeq,
                                icDCEvent,
                                icSource,
                                ideActStamp,
                                OUTPUT ocError).
      IF oiRequest = 0 THEN UNDO create_upsell, RETURN FALSE.
     
   END.

   RETURN TRUE.
END FUNCTION.

&ENDIF

