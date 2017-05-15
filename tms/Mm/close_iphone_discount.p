/* ----------------------------------------------------------------------
  MODULE .......: close_iphone_discount.p
  TASK .........: Close IPhone discount if customer had not
                  paid any previous invoice
  APPLICATION ..: TMS
  AUTHOR .......: Vikas
  CREATED ......: 28.01.13
  Version ......: Yoigo
  ---------------------------------------------------------------------- */

{Syst/commpaa.i}
gcBrand = "1".
katun   = "CRON".
{Func/cparam2.i}
{Func/date.i}

{Syst/eventval.i}

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {Func/lib/eventlog.i}

   DEFINE VARIABLE lhDPMember AS HANDLE NO-UNDO.
   lhDPMember = BUFFER DPMember:HANDLE.
   RUN StarEventInitialize(lhDPMember).
END.

DEF VAR lcIPhoneDiscountRuleIds AS CHAR NO-UNDO.
DEF VAR lcIPhoneDiscountRuleId  AS CHAR NO-UNDO.
DEF VAR liCount                 AS INT  NO-UNDO.
DEF VAR ldaInvDate              AS DATE NO-UNDO.
DEF VAR ldaToDate               AS DATE NO-UNDO.
DEF VAR ldaFromDate             AS DATE NO-UNDO.
DEF VAR lcPromotionPath         AS CHAR NO-UNDO.
DEF VAR ldeStamp                AS DEC  NO-UNDO.
DEF VAR liPeriod                AS INT  NO-UNDO.
DEF VAR lcLogFile               AS CHAR NO-UNDO.
DEF VAR lcDelim                 AS CHAR NO-UNDO INIT "|".

DEF BUFFER bDPMember FOR DPMember.
DEF STREAM sout.

IF DAY(TODAY) = 1 THEN
   ldaToDate = fLastDayOfMOnth(TODAY - 1).
ELSE
   ldaToDate = fLastDayOfMOnth(TODAY).

ASSIGN lcIPhoneDiscountRuleIds = fCParamC("IPHONE_DISCOUNT")
       lcPromotionPath = fCParamC("IPHONE_DISCOUNT_LOG")
       ldaFromDate = DATE(MONTH(ldaToDate),1,YEAR(ldaToDate))
       liPeriod = YEAR(ldaToDate) * 100 + MONTH(ldaToDate)
       ldeStamp = fMakeTS()
       lcLogFile = lcPromotionPath + "/close_iphone_discount_" +
                   STRING(liPeriod) + "_" + STRING(ldeStamp) + ".log".

OUTPUT STREAM sout TO VALUE(lcLogFile).

IF lcIPhoneDiscountRuleIds = "" THEN DO:
   PUT STREAM sout UNFORMATTED "ERROR:Iphone discount(s) not configured" SKIP.
   OUTPUT STREAM sout CLOSE.
   RETURN.
END. /* IF lcIPhoneDiscountRuleIds = "" THEN DO: */

EACH_DISCOUNT:
DO liCount = 1 to NUM-ENTRIES(lcIPhoneDiscountRuleIds):
   lcIPhoneDiscountRuleId = ENTRY(liCount,lcIPhoneDiscountRuleIds).
   FIND FIRST DiscountPlan WHERE
              DiscountPlan.Brand = gcBrand AND
              DiscountPlan.DPRuleID = lcIPhoneDiscountRuleId NO-LOCK NO-ERROR.
   IF NOT AVAIL DiscountPlan THEN DO:
      PUT STREAM sout UNFORMATTED "ERROR:Invalid Iphone Discount: " +
                                  lcIPhoneDiscountRuleId SKIP.
      NEXT EACH_DISCOUNT.
   END. /* IF NOT AVAIL DiscountPlan THEN DO: */

   EACH_DPMember:
   FOR EACH DPMember WHERE
            DPMember.DPId       = DiscountPlan.DPId AND
            DPMember.HostTable  = "MobSub"    AND
            DPMember.ValidFrom <= ldaToDate   AND
            DPMember.ValidTo   >= ldaFromDate AND
            DPMember.ValidTo   >= DPMember.ValidFrom NO-LOCK,
      FIRST MobSub WHERE
            MobSub.MsSeq = INT(DPMember.KeyValue) NO-LOCK,
      FIRST Customer WHERE
            Customer.CustNum = MobSub.CustNum NO-LOCK,
       EACH DCCLI WHERE
            DCCLI.MsSeq = MobSub.MsSeq AND
            DCCLI.DCEvent BEGINS "PAYTERM" AND
            DCCLI.ValidFrom <= DPMember.ValidTo AND
            DCCLI.ValidTo >= DPMember.ValidFrom NO-LOCK:

      ldaInvDate = fLastDayOfMonth(DCCLI.ValidFrom) + 1.

      FOR EACH Invoice WHERE
               Invoice.Brand     = gcBrand AND
               Invoice.CustNum   = Customer.CustNum AND
               Invoice.InvDate  >= ldaInvDate AND
               Invoice.InvType   = 1 AND
               Invoice.Fromdate <= DCCLI.ValidTo AND
               Invoice.ToDate   >= DCCLI.ValidFrom NO-LOCK,
          EACH SubInvoice OF Invoice NO-LOCK:
         IF SubInvoice.CLI <> MobSub.CLI THEN NEXT.

         /* Close Discount if invoice is unpaid */
         IF Invoice.PaymState <> 2 THEN DO:
            FIND FIRST bDPMember WHERE
                       ROWID(bDPMember) = ROWID(DPMember)
                 EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
            IF NOT AVAIL bDPMember THEN DO:
               PUT STREAM sout UNFORMATTED
                   MobSub.CLI              lcDelim
                   STRING(Invoice.CustNum) lcDelim
                   Invoice.ExtInvID        lcDelim
                   STRING(Invoice.InvDate) lcDelim
                   STRING(Invoice.InvAmt)  lcDelim
                   "ERROR:Discount " + DiscountPlan.DPRuleID +
                   " not found." SKIP.
            END. /* IF NOT AVAIL bDPMember THEN DO: */
            ELSE DO:
               /* Log dpmember modification */
               IF llDoEvent THEN RUN StarEventSetOldBuffer(lhDPMember).
               bDPMember.ValidTo = bDPMember.ValidFrom - 1.
               IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhDPMember).

               PUT STREAM sout UNFORMATTED
                   MobSub.CLI              lcDelim
                   STRING(Invoice.CustNum) lcDelim
                   Invoice.ExtInvID        lcDelim
                   STRING(Invoice.InvDate) lcDelim
                   STRING(Invoice.InvAmt)  lcDelim
                   "Discount " + DiscountPlan.DPRuleID + " closed due to " +
                   "unpaid invoice." SKIP.
            END. /* ELSE DO: */
            NEXT EACH_DPMember.
         END. /* IF Invoice.PaymState <> 2 THEN DO: */
      END. /* FOR EACH Invoice WHERE */
   END. /* FOR EACH DPMember WHERE */
END. /* DO liCount = 1 to NUM-ENTRIES(lcIPhoneDiscountRuleIds): */

OUTPUT STREAM sout CLOSE.

