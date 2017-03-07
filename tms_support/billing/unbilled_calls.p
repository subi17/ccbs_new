/* ----------------------------------------------------------------------
  module .......: Mm/unbilled_calls.p
  task .........: create a dump file for unbilled cdrs
  application ..: tms
  author .......: vikas
  created ......: 03.03.11
  version ......: yoigo
---------------------------------------------------------------------- */

{Syst/commpaa.i}
gcbrand = "1".
{Func/timestamp.i}
{Func/cparam2.i}
{Func/coinv.i}
{Func/fvatfact.i}
{Func/ftransdir.i}

DEFINE VARIABLE liUnBillPeriod   AS INTEGER    NO-UNDO.
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
DEFINE VARIABLE lcMobSubStat     AS CHARACTER  NO-UNDO.
DEFINE VARIABLE lcDelimiter      AS CHARACTER  NO-UNDO INITIAL "|".
DEFINE VARIABLE ldCallPeriodLastDate   AS DATE      NO-UNDO.
DEFINE VARIABLE ldMobSubPeriodLastDate AS DATE      NO-UNDO.
DEFINE VARIABLE llCallOnFirstMonth     AS LOGICAL   NO-UNDO.
DEFINE VARIABLE liMobSubActPeriod      AS INTEGER   NO-UNDO.
DEFINE VARIABLE lcSessionParamList     AS CHARACTER  NO-UNDO.
DEFINE VARIABLE liFirstInvSeq          AS INTEGER    NO-UNDO.
DEFINE VARIABLE liLastInvSeq           AS INTEGER    NO-UNDO.

DEFINE STREAM sFile.

/* If they want to dump unbilled CDR's for an specific period */
liUnBillPeriod = INTEGER(fCParamC("UnbilledCallsPeriod")).
IF liUnBillPeriod > 0 THEN
   ASSIGN ldFromDate = fInt2Date(liUnBillPeriod,1)
          ldToDate   = fInt2Date(liUnBillPeriod,2).
ELSE DO:
   /* It will traverse all the CDR's from 200101 period to last billing period */
   ASSIGN liUnBillPeriod = 200101
          ldFromDate = fInt2Date(liUnBillPeriod,1).

   ASSIGN liUnBillPeriod = IF MONTH(TODAY) = 1 THEN (YEAR(TODAY) - 1) * 100 + 12
                           ELSE YEAR(TODAY) * 100 + (MONTH(TODAY) - 1)
          ldToDate = fInt2Date(liUnBillPeriod,2).
END. /* ELSE DO: */

ASSIGN lcOdir     = fCParamC("CDRTransDir")
       lcSdir     = fCParamC("CDRSpoolDir")
       lcTdir     = fCParamC("UnbilledTrackDir")
       lcfilename = "unbilled_calls_" + STRING(liUnBillPeriod)
       lcNumeric  = SESSION:NUMERIC-FORMAT
       SESSION:NUMERIC-FORMAT = "AMERICAN".

IF lcSdir = "" OR lcSdir = ? OR lcOdir = "" OR lcOdir = ? OR
   lcTdir = "" OR lcTdir = ? THEN RETURN.

LcSessionParamList = SESSION:PARAMETER.
IF LcSessionParamList > "" AND NUM-ENTRIES(LcSessionParamList) = 2 THEN DO:
   ASSIGN liFirstInvSeq = INTEGER(ENTRY(1,LcSessionParamList))
          liLastInvSeq  = INTEGER(ENTRY(2,LcSessionParamList)) NO-ERROR.
   IF liFirstInvSeq = 0 OR liFirstInvSeq = ? OR
      liLastInvSeq = 0 OR liLastInvSeq = ? THEN RETURN.
END. /* IF LcSessionParamList > "" THEN DO: */

IF liFirstInvSeq > 0 THEN
   lcfilename = lcfilename + "_" + STRING(liFirstInvSeq) + "_" +
                STRING(liLastInvSeq).

lcFileName = lcSdir + "/" + lcfilename + ".dump".

OUTPUT STREAM sFile TO VALUE(lcfilename).

IF liFirstInvSeq = 0 THEN
   ASSIGN liFirstInvSeq = 1
          liLastInvSeq  = 99999999.

FOR EACH InvSeq WHERE
         InvSeq.InvSeq   >= liFirstInvSeq AND
         InvSeq.InvSeq   <= liLastInvSeq  AND
         InvSeq.FromDate <= ldToDate   AND
         InvSeq.ToDate   >= ldFromDate AND
         InvSeq.Billed    = FALSE NO-LOCK,
    EACH MobCDR WHERE
         MobCDR.InvCust = InvSeq.CustNum AND
         MobCDR.InvSeq  = InvSeq.InvSeq  AND
         MobCDR.MsSeq   = invseq.MsSeq   AND
         MobCDR.DateSt >= ldFromDate     AND
         MobCDR.DateSt <= ldToDate       AND
         MobCDR.ErrorCode = 0 NO-LOCK:

    assign lcBillingPerm = "Allowed"
           lcMobSubStat  = ""
           liReadInLate  = 0
           ldeAmount     = MobCDR.Amount
           ldeDataAmt    = round(((MobCDR.DataIn + MobCDR.DataOut) / 1024 / 1024), 4)
           liCallPeriod  = (YEAR(MobCDR.DateSt) * 100 + (MONTH(MobCDR.DateSt)))
           ldCallPeriodLastDate = fInt2Date(liCallPeriod,2)
           llCallOnFirstMonth   = NO.

    /* Calculate difference in months between call made and read in date */
    IF MobCDR.ReadDate > ldCallPeriodLastDate THEN
       liReadInLate = (YEAR(MobCDR.ReadDate) - YEAR(MobCDR.DateSt)) * 12 +
                      (MONTH(MobCDR.ReadDate) - MONTH(MobCDR.DateSt)).

    FIND FIRST MobSub WHERE
               MobSub.MsSeq = MobCDR.MsSeq NO-LOCK NO-ERROR.
    IF AVAILABLE MobSub THEN
       ASSIGN lcMobSubStat = "Active"
              liMobSubActPeriod = (YEAR(MobSub.ActivationDate) * 100 +
                                  (MONTH(MobSub.ActivationDate)))
              ldMobSubPeriodLastDate = fInt2Date(liMobSubActPeriod,2)
              llCallOnFirstMonth = (MobCDR.DateSt >= MobSub.ActivationDate AND
                                    MobCDR.DateSt <= ldMobSubPeriodLastDate).
    ELSE IF CAN-FIND (FIRST TermMobSub WHERE
                            TermMobSub.MsSeq = MobCDR.MsSeq) THEN
       lcMobSubStat = "Terminated".
    ELSE
       lcMobSubStat = "Not Applicable".

    /* billing Suspended/Prohibited Active/Prohibited Terminated */
    FOR FIRST Limit NO-LOCK USE-INDEX MsSeq WHERE
              Limit.MsSeq     = MobCDR.MsSeq    AND
              Limit.LimitType = 3               AND
              Limit.TMRuleSeq = 0               AND
              Limit.ToDate   >= InvSeq.ToDate   AND
              Limit.LimitID   = 0               AND
              Limit.CustNum   = MobCDR.InvCust  AND
              Limit.LimitAmt  > 0:
        IF Limit.LimitAmt = 1 THEN
           lcBillingPerm = "Suspended".
        ELSE
           lcBillingPerm = "Prohibited".
    END. /* FOR FIRST Limit NO-LOCK USE-INDEX MsSeq WHERE */

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
    lcMobSubStat             lcDelimiter
    llCallOnFirstMonth       skip.

End. /* FOR EACH InvSeq WHERE */

OUTPUT STREAM sFile CLOSE.
UNIX SILENT VALUE("gzip " + lcfilename).
lcfilename = lcfilename + ".gz".

/* Copy the report to Track directory */
fCopy2TargetDir(lcfilename, ".dump", lcTDir).

/* Move the report to OutGoing directory */
fMove2TransDir(lcfilename, ".dump", lcODir).

SESSION:NUMERIC-FORMAT = lcNumeric.
