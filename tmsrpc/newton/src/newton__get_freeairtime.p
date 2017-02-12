/** Fetch all FreeAirTime entries for a subscription.
* 
* @input       msseq;int
* @output      fats;array;FAT information
               discounts;array;Discount information
* @struct      fat_id;string;unique sequence
               fat_group;string;Group code
               fee_type;boolean;true = calls and false = fixed fees
               period;datetime;the first day of the period which is a month
               amount;double;
               percent;double;
               used;double;
               unit;string;Qty or Amt
               transferrable;boolean;
               invnum;int;optional
* @struct      id;string;string;Discount RuleId
               name;string;Discount name
               cc_display;int;CC Display
               unit;string;Discount unit procent or euro
               amount;double;Discount amount
               cli;string;MobSub msisdn number
               period;datetime;Invoice period
               ext_invnumb;string;Invoice number
               used;double;Paid amount of invoice
*/

{fcgi_agent/xmlrpc/xmlrpc_access.i}
{Syst/commpaa.i}
gcBrand = "1".
katun = "NewtonRPC".

{Func/date.i}
{Func/fixedfee.i}

/* Input parameters */
DEF VAR piMsseq AS INT NO-UNDO.
/* Output parameters */
DEF VAR lcFATArray AS CHAR NO-UNDO.
DEF VAR lcDiscountArray AS CHAR NO-UNDO.
DEF VAR resp_struct AS CHAR NO-UNDO.
DEF VAR lcStruct AS CHAR NO-UNDO.
/* Local variables */
DEF VAR lcc AS CHAR NO-UNDO.
DEF VAR lcMonth AS CHAR NO-UNDO.
DEF VAR lcYear  AS CHAR NO-UNDO.
DEF VAR liCustNum AS INT  NO-UNDO.
DEF VAR liNumberOfMonths AS INT NO-UNDO.
DEF VAR liCount AS INT NO-UNDO.
DEF VAR ldaFromDate AS DATE No-UNDO.
DEF VAR ldaToDate   AS DATE No-UNDO.

FUNCTION fCalculateNumberOfMonths RETURNS INTEGER
   (INPUT idavalidfrom AS DATE, 
    INPUT idavalidto   AS DATE):

   DEF VAR liYear AS INT NO-UNDO.
   DEF VAR liMonth AS INT NO-UNDO.

   IF (idavalidto - idavalidfrom) < 0 THEN RETURN -1.

   ASSIGN liYear = YEAR(idavalidto) - YEAR(idavalidfrom)
          liMonth = MONTH(idavalidto) - MONTH(idavalidfrom).

   IF liYear > 0 THEN DO:
      liYear = liYear * 12.
      liMonth = liMonth + liYear.
   END. /* IF liYear > 0 THEN DO: */

   RETURN liMonth.

END FUNCTION. /* FUNCTION fCalculateNumberOfMonths RETURNS INTEGER */


IF validate_request(param_toplevel_id, "int") EQ ? THEN RETURN.
piMsseq = get_pos_int(param_toplevel_id, "0").
IF gi_xmlrpc_error NE 0 THEN RETURN.

resp_struct = add_struct(response_toplevel_id, "").
lcFATArray = add_array(resp_struct, "fat").
lcDiscountArray = add_array(resp_struct, "Discount").

{newton/src/findtenant.i NO OrderCanal MobSub MsSeq piMsSeq}

liCustNum = MobSub.CustNum.

/* For FATs */
FOR EACH Fatime NO-LOCK
WHERE fatime.brand = "1" AND
      fatime.msseq EQ piMsseq:
   RUN pAddStructFAT.
END.

FOR EACH Fatime NO-LOCK
WHERE fatime.custnum EQ mobsub.custnum:
   IF fatime.msseq EQ piMsseq THEN NEXT.
   RUN pAddStructFAT.
END.

/* For Discounts with certain CLI */
FOR EACH DPMember NO-LOCK WHERE
         DPMember.HostTable = "MobSub" AND
         DPMember.KeyValue  = STRING(piMsSeq):
   liNumberOfMonths = fCalculateNumberOfMonths(INPUT DPMember.ValidFrom,
                                               INPUT DPMember.ValidTo).

   DO liCount = 0 to liNumberOfMonths:
      ASSIGN ldaFromDate = ADD-INTERVAL(DPMember.ValidFrom,liCount,"months")
             ldaFromDate = DATE(MONTH(ldaFromDate),1,YEAR(ldaFromDate))
             ldaToDate   = fLastDayOfMonth(ldaFromDate).
      RUN pAddStructDiscount(INPUT ldaFromDate,INPUT ldaToDate).
   END. /* DO ind = 0 to liNumberOfMonths: */
END. /* FOR EACH DPMember NO-LOCK WHERE */

/* For Discounts with same customer */
FOR EACH MobSub WHERE
         MobSub.Brand = "1" AND
         MobSub.CustNum = liCustNum NO-LOCK:

   IF MobSub.MsSeq EQ piMsseq THEN NEXT.

   FOR EACH DPMember NO-LOCK WHERE
            DPMember.HostTable = "MobSub" AND
            DPMember.KeyValue  = STRING(MobSub.MsSeq):
      liNumberOfMonths = fCalculateNumberOfMonths(INPUT DPMember.ValidFrom,
                                                  INPUT DPMember.ValidTo).

      DO liCount = 0 to liNumberOfMonths:
         ASSIGN ldaFromDate = ADD-INTERVAL(DPMember.ValidFrom,liCount,"months")
                ldaFromDate = DATE(MONTH(ldaFromDate),1,YEAR(ldaFromDate))
                ldaToDate   = fLastDayOfMonth(ldaFromDate).
         RUN pAddStructDiscount(INPUT ldaFromDate,INPUT ldaToDate).
      END. /* DO ind = 0 to liNumberOfMonths: */
   END. /* FOR EACH DPMember NO-LOCK WHERE */
END. /* FOR EACH MobSub WHERE */


PROCEDURE pAddStructFAT:
   lcc = STRING(fatime.period, "999999").
   lcMonth = SUBSTRING(lcc, 5, 2).
   lcYear = SUBSTRING(lcc, 1, 4).

   lcStruct = add_struct(lcFATArray, "").
   add_string(lcStruct, "fat_id", STRING(fatime.fatnum)).
   add_string(lcStruct, "fat_group", fatime.ftgrp).
   add_boolean(lcStruct, "fee_type", fatime.fatclass).
   add_timestamp(lcStruct, "period", DECIMAL(lcYear + lcMonth + '01')).
   add_double(lcStruct, "amount", fatime.amt).
   add_double(lcStruct, "percent", fatime.fatperc).
   add_double(lcStruct, "used", fatime.used).
   add_string(lcStruct, "unit", fatime.QtyUnit).
   add_boolean(lcStruct, "transferrable", fatime.transfer).
   add_string(lcStruct, "cli", fatime.cli).
   IF fatime.invnum GT 0 THEN DO:
      add_int(lcStruct, "invnum", fatime.invnum).
      FIND FIRST invoice NO-LOCK WHERE
                 invoice.invnum = fatime.invnum NO-ERROR.
      IF AVAILABLE invoice THEN
         add_string(lcStruct, "ext_invnum", invoice.ExtInvID).
      ELSE
         add_string(lcStruct, "ext_invnum", STRING(fatime.invnum)).
   END.
END PROCEDURE.


PROCEDURE pAddStructDiscount:

   DEF INPUT PARAMETER idaValidFrom AS DATE NO-UNDO.
   DEF INPUT PARAMETER idaValidTo   AS DATE NO-UNDO.

   DEF VAR liperiod     AS INT NO-UNDO.
   DEF VAR ldeTotalAmt  AS DEC NO-UNDO.
   DEF VAR lcExtInvId   AS CHAR NO-UNDO.

   FIND FIRST DiscountPlan WHERE
              DiscountPlan.DPId = DPMember.DPId NO-LOCK NO-ERROR.

   lcStruct = add_struct(lcDiscountArray,"").

   IF AVAIL DiscountPlan THEN DO:
      add_string(lcStruct, "id", DiscountPlan.DPRuleID).
      add_string(lcStruct, "name", DiscountPlan.DPName).
      add_int(lcStruct, "cc_display", DiscountPlan.CCDisplay).
      add_string(lcStruct, "unit", DiscountPlan.DPUnit).
   END. /* IF AVAIL DiscountPlan THEN DO: */

   add_double(lcStruct, "amount", DPMember.DiscValue).
   add_string(lcStruct, "cli", MobSub.CLI).

   ASSIGN liperiod = YEAR(idaValidFrom) * 100 + MONTH(idaValidFrom)
          lcc = STRING(liperiod, "999999")
          lcMonth = SUBSTRING(lcc, 5, 2)
          lcYear = SUBSTRING(lcc, 1, 4).

   add_timestamp(lcStruct, "period", DECIMAL(lcYear + lcMonth + '01')).

   IF DiscountPlan.BillCode > "" THEN DO:
      ldeTotalAmt = 0.
      FOR EACH Invoice NO-LOCK WHERE
               Invoice.Brand = "1"                    AND
               Invoice.CustNum = MobSub.CustNum       AND
               Invoice.InvType = 1                    AND
               Invoice.FromDate <= idaValidTo   AND
               Invoice.ToDate   >= idaValidFrom,
          EACH SubInvoice NO-LOCK WHERE
               SubInvoice.InvNum = Invoice.InvNum AND
               SubInvoice.MsSeq  = MobSub.MsSeq,
          EACH InvRow NO-LOCK WHERE
               InvRow.InvNum    = Invoice.InvNum AND
               InvRow.SubInvNum = SubInvoice.SubInvNum AND
               InvRow.BillCode  = DiscountPlan.BillCode:

         ASSIGN 
               ldeTotalAmt = ldeTotalAmt + InvRow.Amt
               lcExtInvId = Invoice.ExtInvID.
      END. /* FOR EACH Invoice NO-LOCK WHERE */
      ldeTotalAmt = ABS(ldeTotalAmt).
      add_string(lcStruct, "ext_invnum", lcExtInvId).
      add_double(lcStruct, "used", ldeTotalAmt).
   END. /* IF DiscountPlan.BillCode > "" THEN DO: */

END PROCEDURE.


FINALLY:
   IF VALID-HANDLE(ghFunc1) THEN DELETE OBJECT ghFunc1 NO-ERROR. 
END.
