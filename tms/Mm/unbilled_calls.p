/* ----------------------------------------------------------------------
  module .......: Mm/unbilled_calls.p
  task .........: create a dump file for unbilled cdrs
  application ..: tms
  author .......: vikas
  created ......: 03.03.11
  Modified .....: 
  version ......: yoigo
---------------------------------------------------------------------- */

{Syst/commali.i}
{Func/timestamp.i}
{Func/cparam2.i}
{Func/coinv.i}
{Func/fvatfact.i}
{Func/ftransdir.i}
{Syst/funcrunprocess_update.i}
{Inv/old_unbilled_events.i}

DEFINE INPUT  PARAMETER iiFRProcessID      AS INT  NO-UNDO.
DEFINE INPUT  PARAMETER iiUpdateInterval   AS INT  NO-UNDO.
DEFINE INPUT  PARAMETER iiBillPeriod       AS INT  NO-UNDO.
DEFINE INPUT  PARAMETER idaOldDBTo         AS DATE NO-UNDO.
DEFINE OUTPUT PARAMETER oiHandled          AS INT  NO-UNDO.

DEFINE VARIABLE ldUnBillPeriod             AS DATE       NO-UNDO.
DEFINE VARIABLE liCallPeriod               AS INTEGER    NO-UNDO.
DEFINE VARIABLE ldToDate                   AS DATE       NO-UNDO.
DEFINE VARIABLE liReadInLate               AS INTEGER    NO-UNDO.
DEFINE VARIABLE lcBillingPerm              AS CHARACTER  NO-UNDO.
DEFINE VARIABLE ldeDataAmt                 AS DECIMAL    NO-UNDO.
DEFINE VARIABLE ldeAmount                  AS DECIMAL    NO-UNDO.
DEFINE VARIABLE ldVatFactor                AS DECIMAL    NO-UNDO.
DEFINE VARIABLE lcFileName                 AS CHARACTER  NO-UNDO.
DEFINE VARIABLE lcOdir                     AS CHARACTER  NO-UNDO.
DEFINE VARIABLE lcSdir                     AS CHARACTER  NO-UNDO.
DEFINE VARIABLE lcTdir                     AS CHARACTER  NO-UNDO.
DEFINE VARIABLE lcNumeric                  AS CHARACTER  NO-UNDO.
DEFINE VARIABLE lcTaxZone                  AS CHARACTER  NO-UNDO.
DEFINE VARIABLE lcMobSubStat               AS CHARACTER  NO-UNDO.
DEFINE VARIABLE lcDelimiter                AS CHARACTER  NO-UNDO INITIAL "|".
DEFINE VARIABLE ldCallPeriodLastDate       AS DATE       NO-UNDO.
DEFINE VARIABLE ldMobSubPeriodLastDate     AS DATE       NO-UNDO.
DEFINE VARIABLE llCallOnFirstMonth         AS LOGICAL    NO-UNDO.
DEFINE VARIABLE liMobSubActPeriod          AS INTEGER    NO-UNDO.
DEFINE VARIABLE lhsub                      AS HANDLE     NO-UNDO.
DEFINE VARIABLE ldaOldEventDate            AS DATE       NO-UNDO.
  
DEFINE STREAM sFile.

ASSIGN ldUnBillPeriod = fInt2Date(iiBillPeriod,1)
       ldToDate       = fInt2Date(iiBillPeriod,2)
       lcOdir         = fCParamC("CDRTransDir")
       lcSdir         = fCParamC("CDRSpoolDir")
       lcTdir         = fCParamC("UnbilledTrackDir")
       lcfilename     = "unbilled_calls_" + STRING(iiBillPeriod) + ".dump"
       lcNumeric      = SESSION:NUMERIC-FORMAT
       SESSION:NUMERIC-FORMAT = "AMERICAN"
       lhsub          = BUFFER Mobsub:HANDLE.

IF lcSdir = "" OR lcSdir = ? THEN
   lcSdir = "/store/riftp/billreport/spool".
IF lcOdir = "" OR lcOdir = ? THEN
   lcOdir = "/store/riftp/billreport/processed".
IF lcTdir = "" OR lcTdir = ? THEN
   lcTdir = "/mnt/qss/unbilled_calls".

lcFileName = lcSdir + "/" + lcfilename.

ldaOldEventDate = fOldUnbilledEventLimit(1).
IF ldaOldEventDate = ? THEN ldaOldEventDate = 1/1/2006.

OUTPUT STREAM sFile TO VALUE(lcfilename).

FOR EACH MsOwner WHERE
         MsOwner.PayType = FALSE NO-LOCK
    BREAK BY MsOwner.MsSeq
          BY MsOwner.InvCust:
   IF FIRST-OF(MsOwner.InvCust) THEN DO:
      FOR EACH InvSeq WHERE
               InvSeq.MsSeq    = MsOwner.MsSeq     AND
               InvSeq.CustNum  = MsOwner.InvCust   AND
               InvSeq.Billed   = FALSE             AND
               InvSeq.ToDate  >  ldaOldEventDate   AND
               InvSeq.ToDate  <= ldToDate NO-LOCK:

         ASSIGN lcBillingPerm = "Allowed"
                lcMobSubStat  = "".

         FIND FIRST MobSub WHERE
                    MobSub.MsSeq = InvSeq.MsSeq NO-LOCK NO-ERROR.
         IF AVAILABLE MobSub THEN
            ASSIGN lcMobSubStat = "Active"
                   liMobSubActPeriod = (YEAR(MobSub.ActivationDate) * 100 +
                                       (MONTH(MobSub.ActivationDate)))
                   ldMobSubPeriodLastDate = fInt2Date(liMobSubActPeriod,2)
                   lhSub = BUFFER MobSub:HANDLE.
         ELSE DO:
            FIND FIRST TermMobSub WHERE
                       TermMobSub.MsSeq = InvSeq.MsSeq NO-LOCK NO-ERROR.
            IF AVAILABLE TermMobSub THEN
               ASSIGN
                  lcMobSubStat = "Terminated"
                  liMobSubActPeriod = (YEAR(TermMobSub.ActivationDate) * 100 +
                                      (MONTH(TermMobSub.ActivationDate)))
                  ldMobSubPeriodLastDate = fInt2Date(liMobSubActPeriod,2)
                  lhSub = BUFFER TermMobSub:HANDLE.
         END. /* ELSE DO: */

         /* billing Suspended/Prohibited Active/Prohibited Terminated */
         FOR FIRST Limit NO-LOCK USE-INDEX MsSeq WHERE
                   Limit.MsSeq     = InvSeq.MsSeq    AND
                   Limit.LimitType = 3               AND
                   Limit.TMRuleSeq = 0               AND
                   Limit.FromDate <= InvSeq.ToDate   AND
                   Limit.ToDate   >= InvSeq.FromDate AND
                   Limit.LimitID   = 0               AND
                   Limit.CustNum   = InvSeq.CustNum  AND
                   Limit.LimitAmt  > 0:
            IF Limit.LimitAmt = 1 THEN lcBillingPerm = "Suspended".
            ELSE lcBillingPerm = "Prohibited".
         END. /* FOR FIRST Limit NO-LOCK USE-INDEX MsSeq WHERE */

         IF CONNECTED("mcdr") THEN
            RUN pCollectCDR("mcdr").
         IF InvSeq.FromDate <= idaOldDBTo AND CONNECTED("oldmcdr") THEN 
            RUN pCollectCDR("oldmcdr").

         
      END. /* FOR EACH InvSeq WHERE */
   END. /* IF FIRST-OF(MsOwner.InvCust) THEN DO: */
END. /* FOR EACH MsOwner WHERE */

OUTPUT STREAM sFile CLOSE.
UNIX SILENT VALUE("gzip " + lcfilename).
lcfilename = lcfilename + ".gz".

/* Copy the report to Track directory */
fCopy2TargetDir(lcfilename, ".dump", lcTDir).

/* Move the report to OutGoing directory */
fMove2TransDir(lcfilename, ".dump", lcODir).

SESSION:NUMERIC-FORMAT = lcNumeric.


PROCEDURE pCollectCDR:

   DEFINE INPUT PARAMETER icDB AS CHARACTER NO-UNDO.  

   DEF VAR lhWorkCDRdb      AS HANDLE NO-UNDO.
   DEF VAR lhFind           AS HANDLE NO-UNDO.
   DEF VAR lcFind           AS CHAR   NO-UNDO.
   DEF VAR lcCDRdb          AS CHAR   NO-UNDO.

   IF icDB = "mcdr" THEN 
      lcCDRdb = "mcdr.MobCDR".
   ELSE IF icDB = "oldmcdr" THEN 
      lcCDRdb = "oldmcdr.MobCDR".

   CREATE BUFFER lhWorkCDRdb FOR TABLE lcCDRdb.
 
   CREATE QUERY lhFind.
   lhFind:FORWARD-ONLY = TRUE.
   lhFind:SET-BUFFERS(lhWorkCDRdb).

   lcFind = 'FOR EACH ' + 
            lcCDRdb + ' NO-LOCK WHERE ' +
            lcCDRdb + '.InvCust  = ' + STRING(InvSeq.CustNum)   + ' AND ' +
            lcCDRdb + '.InvSeq   = ' + STRING(InvSeq.InvSeq)    + ' AND ' +
            lcCDRdb + '.DateST  >= ' + STRING(InvSeq.FromDate)  + ' AND ' +
            lcCDRdb + '.DateST  <= ' + STRING(InvSeq.ToDate)    + '     '.
   
   lhFind:QUERY-PREPARE(lcFind).
   lhFind:QUERY-OPEN.
      
   REPEAT:
      lhFind:GET-NEXT.
      IF lhFind:QUERY-OFF-END THEN LEAVE.

      IF lhWorkCDRdb::ErrorCode NE 0 THEN NEXT.

      ASSIGN liReadInLate  = 0
             ldeAmount     = lhWorkCDRdb::Amount
             ldeDataAmt    = round(((lhWorkCDRdb::DataIn + lhWorkCDRdb::DataOut) /
                                    1024 / 1024), 4)
             liCallPeriod  = (YEAR(lhWorkCDRdb::DateSt) * 100 +
                             (MONTH(lhWorkCDRdb::DateSt)))
             ldCallPeriodLastDate = fInt2Date(liCallPeriod,2)
             llCallOnFirstMonth = (lhWorkCDRdb::DateSt >= (IF lhSub:AVAILABLE THEN lhSub::ActivationDate
                                                                               ELSE ?) AND
                                  lhWorkCDRdb::DateSt <= ldMobSubPeriodLastDate)
             oiHandled = oiHandled + 1.

      IF iiUpdateInterval > 0 AND oiHandled MOD iiUpdateInterval = 0
      THEN DO:
         IF NOT fUpdateFuncRunProgress(iiFRProcessID,oiHandled) THEN
            RETURN "ERROR:Stopped".
      END. /* IF iiUpdateInterval > 0 AND oiHandled */

      /* Difference in months between call made and read in date */
      IF lhWorkCDRdb::ReadDate > ldCallPeriodLastDate THEN
         liReadInLate = (YEAR(lhWorkCDRdb::ReadDate) - YEAR(lhWorkCDRdb::DateSt)) * 12 +
                        (MONTH(lhWorkCDRdb::ReadDate) - MONTH(lhWorkCDRdb::DateSt)).

      IF lhWorkCDRdb::VatIncl THEN DO:
         FIND FIRST Customer WHERE
                    Customer.CustNum = lhWorkCDRdb::InvCust NO-LOCK NO-ERROR.
         IF AVAILABLE Customer THEN
            lcTaxZone = fRegionTaxZone(Customer.Region).
          
         ldVatFactor = fVatFactor(IF AVAILABLE Customer THEN
                                  Customer.VATUsage ELSE 1,
                                  lcTaxZone,
                                  lhWorkCDRdb::BillCode,
                                  TODAY - 1).
         ldeAmount = (ldeAmount / ldVatFactor).
      END. /* IF lhWorkCDRdb::VatIncl THEN DO: */

      /* Dump the record in output file */
      PUT STREAM sFile UNFORMATTED
         lhWorkCDRdb::DateSt            lcDelimiter
         lhWorkCDRdb::ReadDate          lcDelimiter
         lhWorkCDRdb::MsSeq             lcDelimiter
         lhWorkCDRdb::CLI               lcDelimiter
         lhWorkCDRdb::CLITYPE           lcDelimiter
         lhWorkCDRdb::CCN               lcDelimiter
         lhWorkCDRdb::BillCode          lcDelimiter
         lhWorkCDRdb::BillDur           lcDelimiter
         ldeDataAmt                     lcDelimiter
         round(ldeAmount, 2)            lcDelimiter
         liReadInLate                   lcDelimiter
         lcBillingPerm                  lcDelimiter
         lcMobSubStat                   lcDelimiter
         llCallOnFirstMonth             SKIP.

   END.

   lhFind:QUERY-CLOSE.
 
   FINALLY:
      DELETE OBJECT lhFind NO-ERROR.
      DELETE OBJECT lhWorkCDRdb NO-ERROR.
   END FINALLY.

END PROCEDURE.
