{commpaa.i}
gcbrand = "1".
katun = "Qvantel".

DEF VAR ldReqAmt  AS DEC  NO-UNDO.
DEF VAR ldaDate   AS DATE NO-UNDO.
DEF VAR lcReqChar AS CHAR NO-UNDO.
DEF VAR llclosed  AS LOG  NO-UNDO.
DEF VAR ldAmt     AS DEC  NO-UNDO INIT 3.
DEF VAR licount   AS INT  NO-UNDO.

ldaDate = 06/01/2014.

OUTPUT TO "/apps/yoigo/tms_support/testing/contm2_fee_change.txt" append.

EACH_MOBSUB:
FOR FIRST DayCampaign WHERE
          DayCampaign.Brand = gcBrand AND
          DayCampaign.DCEvent = "CONTM2" NO-LOCK,
    EACH DCCLI WHERE
         DCCLI.Brand = gcBrand AND
         DCCLI.DCEvent = "CONTM2" AND
         DCCLI.ValidTo >= ldaDate NO-LOCK,
   FIRST MobSub WHERE
         MobSub.MsSeq = DCCLI.MsSeq NO-LOCK:

   llclosed = FALSE.
   licount = licount + 1.

   STATUS DEFAULT STRING(licount).

   FOR EACH FixedFee NO-LOCK USE-INDEX HostTable WHERE
            FixedFee.Brand     = gcBrand   AND 
            FixedFee.HostTable = "MobSub"  AND
            FixedFee.KeyValue  = STRING(MobSub.MsSeq) AND
            FixedFee.CalcObj   = DCCLI.DCEvent:

      IF FixedFee.Amt <> ldAmt THEN NEXT EACH_MOBSUB.

      RUN closefee.p(FixedFee.FFNum,
                     (ldaDate - 1),
                     FALSE, /* credit billed fees */
                     TRUE,
                     MobSub.MsSeq,
                     "", /* Data bundle id */
                     katun, /* eventlog.usercode */
                     "SummerCampaign-PriceChange", /* eventlog.memo */
                     0,
                     OUTPUT ldReqAmt).
      IF RETURN-VALUE > "" THEN NEXT.
      llclosed = TRUE.
   END.

   IF llclosed = FALSE THEN DO:
      PUT UNFORMATTED "Fixed Fee not closed:" STRING(MobSub.MsSeq) SKIP.
      NEXT.
   END.

   RUN creasfee.p(MobSub.CustNum,
                  MobSub.MsSeq,
                  (IF ldaDate > DCCLI.ValidFrom THEN ldaDate ELSE DCCLI.ValidFrom),
                  "FeeModel",
                  DayCampaign.FeeModel,
                  9,
                  ?,
                  DCCLI.DCEvent + " created " + 
                  STRING(TODAY,"99.99.9999") +  /* memo */
                  "¤" +  DCCLI.DCEvent ,  /* calcobject */
                  FALSE,              /* no messages to screen */
                  katun,
                  "SummerCampaign-PriceChange",
                  0, /* order id */
                  "DCCLI",
                  STRING(DCCLI.PerContractId),
                  OUTPUT lcReqChar).

   IF lcReqChar BEGINS "ERROR:" OR lcReqChar BEGINS "0" THEN
      PUT UNFORMATTED "Fixed Fee not created:" STRING(MobSub.MsSeq) SKIP.
   ELSE
      PUT UNFORMATTED "Fixed Fee is created:" STRING(MobSub.MsSeq) SKIP.
END.