/* ----------------------------------------------------------------------
  Module .......: Mm/billed_prev_calls.p
  Task .........: Dump all the Calls which are made before current billing
                  period however its not billed previously but billed in
                  current billing period
  Application ..: TMS
  Author .......: Vikas
  Created ......: 03.03.11
  Modified .....: 
  Version ......: Yoigo
---------------------------------------------------------------------- */

{Syst/commali.i}
{Func/timestamp.i}
{Func/cparam2.i}
{Func/coinv.i}
{Func/fvatfact.i}
{Func/ftransdir.i}
{Syst/funcrunprocess_update.i}

DEFINE INPUT  PARAMETER iiInvType          AS INT  NO-UNDO.
DEFINE INPUT  PARAMETER iiBillPeriod       AS INT  NO-UNDO.
DEFINE INPUT  PARAMETER iiFRProcessID      AS INT  NO-UNDO.
DEFINE INPUT  PARAMETER iiUpdateInterval   AS INT  NO-UNDO.
DEFINE INPUT  PARAMETER idaOldDBTo         AS DATE NO-UNDO.
DEFINE OUTPUT PARAMETER oiHandled          AS INT  NO-UNDO.

DEFINE VARIABLE liCallPeriod     AS INTEGER    NO-UNDO.
DEFINE VARIABLE ldFromDate       AS DATE       NO-UNDO.
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
DEFINE VARIABLE liInvPeriod            AS INTEGER   NO-UNDO.
DEFINE VARIABLE ldInvFromDate          AS DATE      NO-UNDO.
DEFINE VARIABLE ldInvToDate            AS DATE      NO-UNDO.
DEFINE VARIABLE lhsub                  AS HANDLE    NO-UNDO.

DEFINE STREAM sFile.

ASSIGN ldFromDate = fInt2Date(iiBillPeriod,1)
       lcOdir     = fCParamC("CDRTransDir")
       lcSdir     = fCParamC("CDRSpoolDir")
       lcTdir     = fCParamC("BilledTrackDir")
       lcfilename = "billed_prev_calls_" + STRING(iiBillPeriod) + ".dump"
       lcNumeric  = SESSION:NUMERIC-FORMAT
       SESSION:NUMERIC-FORMAT = "AMERICAN"
       liInvPeriod    = YEAR(TODAY) * 100 + MONTH(TODAY)
       ldInvFromDate  = fInt2Date(liInvPeriod,1)
       ldInvToDate    = fInt2Date(liInvPeriod,2)
       lhsub          = BUFFER Mobsub:HANDLE.

IF lcSdir = "" OR lcSdir = ? THEN
   lcSdir = "/store/riftp/billreport/spool".
IF lcOdir = "" OR lcOdir = ? THEN
   lcOdir = "/store/riftp/billreport/processed".
IF lcTdir = "" OR lcTdir = ? THEN
   lcTdir = "/mnt/qss/billed_prev_calls".

lcFileName = lcSdir + "/" + lcfilename.

OUTPUT STREAM sFile TO VALUE(lcfilename).

FOR EACH Invoice WHERE
         Invoice.Brand    = gcBrand       AND
         Invoice.InvDate >= ldInvFromDate AND
         Invoice.InvDate <= ldInvToDate   AND
         Invoice.InvType  = iiInvType NO-LOCK,
    EACH InvRow OF Invoice WHERE
         InvRow.FromDate < ldFromDate AND
         InvRow.RowType = 2 NO-LOCK,
   FIRST SubInvoice WHERE
         SubInvoice.InvNum    = Invoice.InvNum AND
         SubInvoice.SubInvNum = InvRow.SubInvNum NO-LOCK,
   FIRST InvSeq WHERE InvSeq.InvSeq = SubInvoice.InvSeq:

   lcBillingPerm = "Allowed".

   /* Indentify whether call was made on First month of subs. activation */
   FIND FIRST MobSub WHERE
              MobSub.MsSeq = InvSeq.MsSeq NO-LOCK NO-ERROR.
   IF AVAILABLE MobSub THEN
      ASSIGN liMobSubActPeriod = (YEAR(MobSub.ActivationDate) * 100 +
                                 (MONTH(MobSub.ActivationDate)))
             ldMobSubPeriodLastDate = fInt2Date(liMobSubActPeriod,2)
             lhSub = BUFFER MobSub:HANDLE.
   ELSE DO:
      FIND FIRST TermMobSub WHERE
                 TermMobSub.MsSeq = InvSeq.MsSeq NO-LOCK NO-ERROR.
      IF AVAILABLE TermMobSub THEN
         ASSIGN liMobSubActPeriod = (YEAR(TermMobSub.ActivationDate) * 100 +
                                    (MONTH(TermMobSub.ActivationDate)))
                ldMobSubPeriodLastDate = fInt2Date(liMobSubActPeriod,2)
                lhSub = BUFFER TermMobsub:HANDLE.             
   END. /* ELSE DO: */

   /* Billing Permissions - Suspended/Prohibited */
   FOR FIRST Limit NO-LOCK USE-INDEX MsSeq WHERE
             Limit.MsSeq     = InvSeq.MsSeq     AND
             Limit.LimitType = 3                AND
             Limit.TMRuleSeq = 0                AND
             Limit.LimitID   = 0                AND
             Limit.ToDate   >= InvRow.FromDate  AND
             Limit.ToDate   <= InvRow.ToDate    AND
             Limit.CustNum   = InvSeq.CustNum   AND
             Limit.LimitAmt  > 0:
      IF Limit.LimitAmt = 1 THEN lcBillingPerm = "Suspended".
      ELSE lcBillingPerm = "Prohibited".
   END. /* FOR LAST Limit NO-LOCK USE-INDEX MsSeq WHERE */

   IF CONNECTED("mcdr") THEN
      RUN pCollectCDR("mcdr").
   IF InvRow.FromDate <= idaOldDBTo AND CONNECTED("oldmcdr") THEN 
      RUN pCollectCDR("oldmcdr").

END. /* FOR EACH Invoice WHERE */

OUTPUT STREAM sFile CLOSE.
UNIX SILENT VALUE("gzip " + lcfilename).
lcfilename = lcfilename + ".gz".

/* Copy the report to Track directory */
fCopy2TargetDir(lcfilename, ".dump", lcTDir).

/* Move the report to Transfer directory */
fMove2TransDir(lcfilename, ".dump", lcODir).

SESSION:NUMERIC-FORMAT = lcNumeric.


PROCEDURE pCollectCDR:

   DEFINE INPUT PARAMETER icDB AS CHARACTER NO-UNDO.  

   DEF VAR lhWorkCDRdb AS HANDLE NO-UNDO.
   DEF VAR lhFind      AS HANDLE NO-UNDO.
   DEF VAR lcFind      AS CHAR   NO-UNDO.
   DEF VAR lcCDRdb     AS CHAR   NO-UNDO.

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
            lcCDRdb + '.InvCust  = ' + STRING(InvSeq.CustNum)  + '  AND ' +
            lcCDRdb + '.InvSeq   = ' + STRING(InvSeq.InvSeq)   + '  AND ' +
            lcCDRdb + '.CLI      = "' +  SubInvoice.CLI  + '"  AND ' +
            lcCDRdb + '.DateST  >= ' + STRING(InvRow.FromDate) + '  AND ' +
            lcCDRdb + '.DateST  <= ' + STRING(InvRow.ToDate)   + '  AND ' +
            lcCDRdb + '.BillCode = "' + InvRow.BillCode + '" '.

   lhFind:QUERY-PREPARE(lcFind).
   lhFind:QUERY-OPEN.

   REPEAT:
      lhFind:GET-NEXT.
      IF lhFind:QUERY-OFF-END THEN LEAVE.

      IF lhWorkCDRdb::ErrorCode NE 0 THEN NEXT.

      ASSIGN
         liReadInLate         = 0
         oiHandled            = oiHandled + 1
         ldeAmount            = lhWorkCDRdb::Amount
         ldeDataAmt           = ROUND(((lhWorkCDRdb::DataIn + lhWorkCDRdb::DataOut)
                                       / 1024 / 1024),4)
         liCallPeriod         = (YEAR(lhWorkCDRdb::DateSt) * 100 + (MONTH(lhWorkCDRdb::DateSt)))
         ldCallPeriodLastDate = fInt2Date(liCallPeriod,2)
         llCallOnFirstMonth   = (lhWorkCDRdb::DateSt >= lhSub::ActivationDate AND
                                 lhWorkCDRdb::DateSt <= ldMobSubPeriodLastDate).

      IF iiUpdateInterval > 0 AND oiHandled MOD iiUpdateInterval = 0 THEN DO:
         IF NOT fUpdateFuncRunProgress(iiFRProcessID,oiHandled) THEN
            RETURN "ERROR:Stopped".
      END. /* IF iiUpdateInterval > 0 AND oiHandled */

      /* Read In Late, if Call ReadDate is later than Call's called period */
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
                                  (TODAY - 1)).
         ldeAmount = (ldeAmount / ldVatFactor).
      END. 

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
         ROUND(ldeAmount, 2)            lcDelimiter
         liReadInLate                   lcDelimiter
         lcBillingPerm                  lcDelimiter
         llCallOnFirstMonth             SKIP.
      
   END.   
   
   lhFind:QUERY-CLOSE.
 
   FINALLY:
      DELETE OBJECT lhFind NO-ERROR.
      DELETE OBJECT lhWorkCDRdb NO-ERROR.
   END FINALLY.
   
END PROCEDURE.
