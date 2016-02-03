{Syst/commpaa.i}
gcbrand = "1".
katun = "Qvantel".

DEF VAR ldReqAmt  AS DEC  NO-UNDO.
DEF VAR ldaDate   AS DATE NO-UNDO.
DEF VAR lcReqChar AS CHAR NO-UNDO.
DEF VAR llclosed  AS LOG  NO-UNDO.
DEF VAR ldAmt     AS DEC  NO-UNDO INIT 6.
DEF VAR liMsSeq   AS INT  NO-UNDO.
DEF VAR liCustNum AS INT  NO-UNDO.
DEF VAR ldaMobActDate AS DATE NO-UNDO.
DEF VAR llActive  AS LOG NO-UNDO.

ldaDate = 06/01/2014.

OUTPUT TO "/apps/yoigo/tms_support/testing/voip_fee_change.txt" append.

EACH_MOBSUB:
FOR FIRST DayCampaign WHERE
          DayCampaign.Brand = gcBrand AND
          DayCampaign.DCEvent = "BONO_VOIP" NO-LOCK,
    FIRST ServiceLimit WHERE
          ServiceLimit.GroupCode = "BONO_VOIP" NO-LOCK,
     EACH MServiceLimit WHERE
          MServiceLimit.SlSeq = ServiceLimit.SlSeq AND
          MServiceLimit.DialType = 7 AND
          MServiceLimit.EndTS >= 20140601 NO-LOCK,
    FIRST MobSub WHERE
          MobSub.MsSeq = MServiceLimit.MsSeq NO-LOCK:


       ASSIGN liMsSeq = MobSub.MsSeq
              liCustNum = MobSub.CustNum
              ldaMobActDate = MobSub.ActivationDate
              llActive = TRUE.
/*
    FIND FIRST MobSub WHERE
               MobSub.MsSeq = MServiceLimit.MsSeq NO-LOCK NO-ERROR.
    IF AVAIL MobSub THEN DO:
       ASSIGN liMsSeq = MobSub.MsSeq
              liCustNum = MobSub.CustNum
              ldaMobActDate = MobSub.ActivationDate
              llActive = TRUE.
      NEXT EACH_MOBSUB.
    END.
    ELSE DO:
       FIND FIRST TermMobSub WHERE
                  TermMobSub.MsSeq = MServiceLimit.MsSeq NO-LOCK NO-ERROR.
       IF NOT AVAIL TermMobSub THEN NEXT EACH_MOBSUB.
       ASSIGN liMsSeq = TermMobSub.MsSeq
              liCustNum = TermMobSub.CustNum
              ldaMobActDate = TermMobSub.ActivationDate
              llActive = FALSE.
    END.
*/
   llclosed = FALSE.

   FOR EACH FixedFee NO-LOCK USE-INDEX HostTable WHERE
            FixedFee.Brand     = gcBrand   AND 
            FixedFee.HostTable = "MobSub"  AND
            FixedFee.KeyValue  = STRING(liMsSeq) AND
            FixedFee.CalcObj   = DayCampaign.DCEvent:

      IF FixedFee.Amt <> ldAmt THEN NEXT EACH_MOBSUB.

      /*IF llActive = FALSE AND FixedFee.CustPP > 0 THEN FixedFee.CustPP = 0.*/

      RUN closefee.p(FixedFee.FFNum,
                     (ldaDate - 1),
                     FALSE, /* credit billed fees */
                     TRUE,
                     liMsSeq,
                     "", /* Data bundle id */
                     katun, /* eventlog.usercode */
                     "SummerCampaign-PriceChange", /* eventlog.memo */
                     0,
                     OUTPUT ldReqAmt).
      IF RETURN-VALUE > "" THEN NEXT.
      llclosed = TRUE.
   END.

   IF llclosed = FALSE THEN DO:
      PUT UNFORMATTED "Fixed Fee not closed:" STRING(liMsSeq) SKIP.
      NEXT.
   END.

   RUN creasfee.p(liCustNum,
                  liMsSeq,
                  (IF ldaDate > ldaMobActDate THEN ldaDate ELSE ldaMobActDate),
                  "FeeModel",
                  DayCampaign.FeeModel,
                  9,
                  ?,
                  DayCampaign.DCEvent + " created " + 
                  STRING(TODAY,"99.99.9999") +  /* memo */
                  "¤" +  DayCampaign.DCEvent ,  /* calcobject */
                  FALSE,              /* no messages to screen */
                  katun,
                  "SummerCampaign-PriceChange",
                  0, /* order id */
                  "",
                  "",
                  OUTPUT lcReqChar).

   IF lcReqChar BEGINS "ERROR:" OR lcReqChar BEGINS "0" THEN
      PUT UNFORMATTED "Fixed Fee not created:" STRING(liMsSeq) SKIP.
   ELSE
      PUT UNFORMATTED "Fixed Fee is created:" STRING(liMsSeq) SKIP.
END.