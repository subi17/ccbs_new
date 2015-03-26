/* ----------------------------------------------------------------------
  Module .......: Mm/billed_prev_calls.p
  Task .........: Dump all the Calls which are made before current billing
                  period however its not billed previously but billed in
                  current billing period
  Application ..: TMS
  Author .......: Vikas
  Created ......: 03.03.11
  Version ......: Yoigo
---------------------------------------------------------------------- */

{commpaa.i}
gcbrand = "1".
{timestamp.i}
{cparam2.i}
{coinv.i}
{fvatfact.i}
{ftransdir.i}

DEFINE VARIABLE liBillPeriod     AS INTEGER    NO-UNDO.
DEFINE VARIABLE liCallPeriod     AS INTEGER    NO-UNDO.
DEFINE VARIABLE ldFromDate       AS DATE       NO-UNDO.
DEFINE VARIABLE ldToDate         AS DATE       NO-UNDO.
DEFINE VARIABLE liReadInLate     AS INTEGER    NO-UNDO.
DEFINE VARIABLE lcBillingPerm    AS CHARACTER  NO-UNDO.
DEFINE VARIABLE ldeDataAmt       AS DECIMAL    NO-UNDO.
DEFINE VARIABLE ldeAmount        AS DECIMAL    NO-UNDO.
DEFINE VARIABLE ldVatFactor      AS DECIMAL    NO-UNDO.
DEFINE VARIABLE lcFileName       AS CHARACTER  NO-UNDO.
DEFINE VARIABLE lcOdir           AS CHARACTER  NO-UNDO.
DEFINE VARIABLE lcSdir           AS CHARACTER  NO-UNDO.
DEFINE VARIABLE lcTdir           AS CHARACTER  NO-UNDO.
DEFINE VARIABLE lcNumeric        AS CHARACTER  NO-UNDO.
DEFINE VARIABLE lcTaxZone        AS CHARACTER  NO-UNDO.
DEFINE VARIABLE lcDelimiter      AS CHARACTER  NO-UNDO INITIAL "|".

DEFINE VARIABLE ldCallPeriodLastDate   AS DATE      NO-UNDO.
DEFINE VARIABLE ldMobSubPeriodLastDate AS DATE      NO-UNDO.
DEFINE VARIABLE llCallOnFirstMonth     AS LOGICAL   NO-UNDO.
DEFINE VARIABLE liMobSubActPeriod      AS INTEGER   NO-UNDO.
DEFINE VARIABLE ldInvPeriodFirstDate   AS DATE      NO-UNDO.
DEFINE VARIABLE liInvPeriod            AS INTEGER   NO-UNDO.
DEFINE VARIABLE ldInvFromDate          AS DATE      NO-UNDO.
DEFINE VARIABLE ldInvToDate            AS DATE      NO-UNDO.

DEFINE STREAM sFile.

/* Dump of CDR's with date previous to the billing period and
   billed in the specified billing period */
liBillPeriod = INTEGER(fCParamC("BilledCallsPeriod")).
IF liBillPeriod > 0 THEN
   ASSIGN ldFromDate = fInt2Date(liBillPeriod,1)
          ldToDate   = fInt2Date(liBillPeriod,2).
ELSE
   /* Dump of CDR's with date previous to the billing period and
      billed in the last billing period */
   ASSIGN liBillPeriod = IF MONTH(TODAY) = 1 THEN (YEAR(TODAY) - 1) * 100 + 12
                         ELSE YEAR(TODAY) * 100 + (MONTH(TODAY) - 1)
          ldFromDate   = fInt2Date(liBillPeriod,1)
          ldToDate     = fInt2Date(liBillPeriod,2).

ASSIGN lcOdir     = fCParamC("CDRTransDir")
       lcSdir     = fCParamC("CDRSpoolDir")
       lcTdir     = fCParamC("BilledTrackDir")
       lcfilename = "billed_prev_calls_" + STRING(liBillPeriod) + ".dump"
       lcNumeric  = SESSION:NUMERIC-FORMAT
       SESSION:NUMERIC-FORMAT = "AMERICAN"
       liInvPeriod    = YEAR(TODAY) * 100 + MONTH(TODAY)
       ldInvFromDate  = fInt2Date(liInvPeriod,1)
       ldInvToDate    = fInt2Date(liInvPeriod,2).

IF lcSdir = "" OR lcSdir = ? OR lcOdir = "" OR lcOdir = ? OR
   lcTdir = "" OR lcTdir = ? THEN RETURN.

lcFileName = lcSdir + "/" + lcfilename.

OUTPUT STREAM sFile TO VALUE(lcfilename).

/* Traverse all the Call's which are made before this billing period
   however its not billed previously but billed in this billing period */
FOR EACH Invoice WHERE
         Invoice.Brand    = gcBrand        AND
         Invoice.InvDate >= ldInvFromDate  AND
         Invoice.InvDate <= ldInvToDate    AND
         Invoice.InvType  = 1 NO-LOCK,
    EACH SubInvoice OF Invoice NO-LOCK,    
    EACH InvSeq WHERE
         InvSeq.InvSeq     = SubInvoice.InvSeq AND
         InvSeq.custnum    = Invoice.custnum   AND
         InvSeq.FromDate   < ldFromDate        AND
         InvSeq.ToDate    >= ldFromDate        AND
         InvSeq.Billed     = TRUE NO-LOCK,
    EACH MobCDR WHERE
         MobCDR.InvCust    = InvSeq.CustNum AND
         MobCDR.InvSeq     = InvSeq.InvSeq NO-LOCK:

    IF MobCDR.MsSeq <> InvSeq.MsSeq OR MobCDR.ErrorCode > 0 OR
       MobCDR.DateSt >= ldFromDate THEN NEXT.

    assign lcBillingPerm = "Allowed"
           liReadInLate  = 0
           ldeAmount     = MobCDR.Amount
           ldeDataAmt    = round(((MobCDR.DataIn + MobCDR.DataOut) / 1024 / 1024), 4)
           liCallPeriod  = (YEAR(MobCDR.DateSt) * 100 + (MONTH(MobCDR.DateSt)))
           ldCallPeriodLastDate = fInt2Date(liCallPeriod,2)
           liInvPeriod  = (YEAR(InvSeq.ToDate) * 100 + (MONTH(InvSeq.ToDate)))
           ldInvPeriodFirstDate = fInt2Date(liInvPeriod,1)
           llCallOnFirstMonth = no.

    /* Read In Late, if Call ReadDate is later than Call's called period */
    IF MobCDR.ReadDate > ldCallPeriodLastDate THEN
       liReadInLate = (YEAR(MobCDR.ReadDate) - YEAR(MobCDR.DateSt)) * 12 +
                      (MONTH(MobCDR.ReadDate) - MONTH(MobCDR.DateSt)).

    /* Indentify whether call was made on First month of subscription activation */
    FIND FIRST MobSub WHERE
               MobSub.MsSeq = MobCDR.MsSeq NO-LOCK NO-ERROR.
    IF AVAILABLE MobSub THEN
       ASSIGN liMobSubActPeriod = (YEAR(MobSub.ActivationDate) * 100 +
                                  (MONTH(MobSub.ActivationDate)))
              ldMobSubPeriodLastDate = fInt2Date(liMobSubActPeriod,2)
              llCallOnFirstMonth = (MobCDR.DateSt >= MobSub.ActivationDate AND
                                    MobCDR.DateSt <= ldMobSubPeriodLastDate).
    ELSE DO:
       FIND FIRST TermMobSub WHERE
                  TermMobSub.MsSeq = MobCDR.MsSeq NO-LOCK NO-ERROR.
       IF AVAILABLE TermMobSub THEN
          ASSIGN liMobSubActPeriod = (YEAR(TermMobSub.ActivationDate) * 100 +
                                     (MONTH(TermMobSub.ActivationDate)))
                 ldMobSubPeriodLastDate = fInt2Date(liMobSubActPeriod,2)
                 llCallOnFirstMonth = (MobCDR.DateSt >= TermMobSub.ActivationDate AND
                                       MobCDR.DateSt <= ldMobSubPeriodLastDate).
    END. /* ELSE DO: */

    /* Billing Permissions - Suspended/Prohibited */
    FOR FIRST Limit NO-LOCK USE-INDEX MsSeq WHERE
              Limit.MsSeq     = MobCDR.MsSeq          AND
              Limit.LimitType = 3                     AND
              Limit.TMRuleSeq = 0                     AND
              Limit.LimitID   = 0                     AND
             (Limit.ToDate   >= ldInvPeriodFirstDate  AND
              Limit.ToDate   <=  InvSeq.ToDate)       AND
              Limit.CustNum   = MobCDR.CustNum        AND
              Limit.LimitAmt  > 0:

        IF Limit.LimitAmt = 1 THEN
           lcBillingPerm = "Suspended".
        ELSE
           lcBillingPerm = "Prohibited".
    END. /* FOR LAST Limit NO-LOCK USE-INDEX MsSeq WHERE */

    IF MobCDR.VatIncl THEN DO:
       FIND FIRST Customer WHERE
                  Customer.CustNum = MobCDR.InvCust NO-LOCK NO-ERROR.
       IF AVAILABLE Customer THEN
          lcTaxZone = fRegionTaxZone(Customer.Region).

       ldVatFactor = fVatFactor(IF AVAILABLE Customer THEN Customer.VATUsage
                                ELSE 1,
                                lcTaxZone,
                                MobCDR.BillCode,
                                TODAY - 1).
       ldeAmount = (ldeAmount / ldVatFactor).
    END. /* IF MobCDR.VatIncl THEN DO: */

    /* Dump the record in output file */
    PUT STREAM sFile UNFORMATTED
    MobCDR.DateSt            lcDelimiter
    MobCDR.ReadDate          lcDelimiter
    MobCDR.MsSeq             lcDelimiter
    MobCDR.CLI               lcDelimiter
    MobCDR.CLITYPE           lcDelimiter
    MobCDR.CCN               lcDelimiter
    MobCDR.BillCode          lcDelimiter
    MobCDR.BillDur           lcDelimiter
    ldeDataAmt               lcDelimiter
    round(ldeAmount, 2)      lcDelimiter
    liReadInLate             lcDelimiter
    lcBillingPerm            lcDelimiter
    llCallOnFirstMonth       skip.

End. /* FOR EACH InvSeq WHERE */

OUTPUT STREAM sFile CLOSE.
UNIX SILENT VALUE("gzip " + lcfilename).
lcfilename = lcfilename + ".gz".

/* Copy the report to Track directory */
fCopy2TargetDir(lcfilename, ".dump", lcTDir).

/* Move the report to Transfer directory */
fMove2TransDir(lcfilename, ".dump", lcODir).

SESSION:NUMERIC-FORMAT = lcNumeric.

