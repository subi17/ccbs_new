DEFINE STREAM sSubs.

DEFINE VARIABLE lcFileName AS CHARACTER NO-UNDO.
DEFINE VARIABLE liBillPeriod AS INTEGER NO-UNDO.
DEFINE VARIABLE ldtValidFrom  AS DATE   NO-UNDO.
DEFINE VARIABLE ldtValidTo    AS DATE   NO-UNDO.

ASSIGN lcFileName = "/apps/xfera/saghanta/yoigo/tms_support/201607/yts_9197_rvtermdt3disc_unbilled.csv".

OUTPUT STREAM sSubs TO VALUE(lcFileName).

PUT STREAM sSubs UNFORMATTED "SubsID,DiscValidFrom,DiscValidTo,DiscBillPeriod,CoversPeriod,RVBillPeriod,Amount,BilledStatus" SKIP.

FOR EACH SingleFee NO-LOCK WHERE
         SingleFee.Brand = "1" AND
         SingleFee.BillCode = "RVTERM":
   ASSIGN liBillPeriod = INT(TRUNCATE(SingleFee.Concerns[1] / 100,0))
          ldtValidFrom = DATE(SingleFee.BillPeriod MOD 100,
                              1,
                              INT(TRUNCATE(SingleFee.BillPeriod / 100,0)))
          ldtValidTo   = ldtValidFrom + 26
          .
   IF ldtValidTo NE ? THEN REPEAT:
      IF MONTH(ldtValidTo) = MONTH(ldtValidFrom) THEN
         ldtValidTo = ldtValidTo + 1.
      ELSE DO:
         ldtValidTo = ldtValidTo - 1.
         LEAVE.
      END.
   END.
   FIND FIRST DPMember NO-LOCK WHERE
              DPMember.HostTable = "MobSub" AND
              DPMember.KeyValue  = SingleFee.KeyValue AND
              DPMember.DPID      = 34 AND 
              DPMember.ValidFrom <= ldtValidTo AND
              DPMember.ValidTo   >= ldtValidFrom AND
              DPMember.ValidTo   >= DPMember.ValidFrom NO-ERROR.
   IF AVAILABLE DPMember THEN DO:
      IF liBillPeriod NE SingleFee.BillPeriod THEN
         PUT STREAM sSubs UNFORMATTED
             DPMember.KeyValue  ","
             DPMember.ValidFrom ","
             DPMember.ValidTo   ","
             liBillPeriod       ","
             SingleFee.Concerns[1] ","
             SingleFee.BillPeriod  ","
             SingleFee.Amt   ","
             SingleFee.Billed
             SKIP.
   END.
END.

OUTPUT STREAM sSubs CLOSE.
