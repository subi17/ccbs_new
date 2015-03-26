/* bundle_first_month_fee.i     19.10.10/aam 
*/

&IF "{&BundleFirstMonthFee}" NE "YES" 
&THEN
&GLOBAL-DEFINE BundleFirstMonthFee YES

{date.i}
{detailvalue.i}
{tmsconst.i}
{cparam2.i}

FUNCTION fCalculateProportionalFee RETURNS DEC
   (idUsage      AS DEC,
    iiInclUnit   AS INT,
    idLimit      AS DEC,
    idBaseAmount AS DEC):
    
   IF iiInclUnit = {&INCLUNIT_MINUTE} THEN
      idUsage = idUsage / 60.
   ELSE IF iiInclUnit = {&INCLUNIT_MEGABYTE} THEN 
      idUsage = idUsage / (1024 * 1024).
   
   IF idLimit > 0 THEN 
      RETURN ROUND(MIN(1,idUsage / idLimit) * idBaseAmount,2).
   ELSE RETURN idBaseAmount.
    
END FUNCTION.

FUNCTION fCalculateFirstMonthFee RETURNS DEC
   (icBrand        AS CHAR,
    iiMsSeq        AS INT, 
    icServiceLimit AS CHAR, 
    idAmount       AS DEC,  /* original amount (full month) */
    iiPeriod       AS INT): /* YYYYMM */

   DEF VAR ldFeeAmount   AS DEC  NO-UNDO.
   DEF VAR ldUsage       AS DEC  NO-UNDO.
   DEF VAR ldPeriodFrom  AS DEC  NO-UNDO.
   DEF VAR ldPeriodTo    AS DEC  NO-UNDO.
   
   DEF BUFFER bfmServiceLCounter FOR ServiceLCounter.
   DEF BUFFER bfmMserviceLimit FOR MServiceLimit.
   DEF BUFFER bfmServiceLimit FOR ServiceLimit.

   ASSIGN
      ldFeeAmount  = 0
      ldPeriodFrom = iiPeriod * 100 + 1
      ldPeriodTo   = iiPeriod * 100 + 32.
   
   FOR EACH bfmServiceLCounter NO-LOCK WHERE
            bfmServiceLCounter.MsSeq  = iiMsSeq  AND
            bfmServiceLCounter.Period = iiPeriod,
      FIRST bfmMServiceLimit NO-LOCK WHERE
            bfmMServiceLimit.MsSeq = iiMsSeq AND
            bfmMServiceLimit.SlSeq = bfmServiceLCounter.SlSeq AND
            bfmMServiceLimit.FromTS >= ldPeriodFrom AND
            bfmMServiceLimit.FromTS <= ldPeriodTo,
      FIRST bfmServiceLimit NO-LOCK WHERE
            bfmServiceLimit.SlSeq = bfmServiceLCounter.SlSeq AND
            bfmServiceLimit.GroupCode = icServiceLimit:
 
      /* % of fee, based on usage */
      ldFeeAmount = fCalculateProportionalFee(bfmServiceLCounter.Amt,
                                              bfmMServiceLimit.InclUnit,
                                              bfmMServiceLimit.InclAmt,
                                              idAmount).
   END.

   RETURN ldFeeAmount. 

END FUNCTION.

FUNCTION fCalculateLastMonthFee RETURNS DEC
   (icBrand        AS CHAR,
    iiMsSeq        AS INT, 
    icServiceLimit AS CHAR, 
    idAmount       AS DEC,  /* original amount (full month) */
    iiPeriod       AS INT): /* YYYYMM */

   DEF VAR ldFeeAmount   AS DEC  NO-UNDO.
   DEF VAR ldUsage       AS DEC  NO-UNDO.
   DEF VAR ldPeriodFrom  AS DEC  NO-UNDO.
   DEF VAR ldPeriodTo    AS DEC  NO-UNDO.
   
   DEF BUFFER bfmServiceLCounter FOR ServiceLCounter.
   DEF BUFFER bfmMserviceLimit FOR MServiceLimit.
   DEF BUFFER bfmServiceLimit FOR ServiceLimit.

   ASSIGN
      ldFeeAmount  = 0
      ldPeriodFrom = iiPeriod * 100 + 1
      ldPeriodTo   = iiPeriod * 100 + 31.
   
   FOR EACH bfmServiceLCounter NO-LOCK WHERE
            bfmServiceLCounter.MsSeq  = iiMsSeq  AND
            bfmServiceLCounter.Period = iiPeriod,
      FIRST bfmMServiceLimit NO-LOCK WHERE
            bfmMServiceLimit.MsSeq = iiMsSeq AND
            bfmMServiceLimit.SlSeq = bfmServiceLCounter.SlSeq AND
            bfmMServiceLimit.FromTS <= ldPeriodTo AND
            bfmMServiceLimit.EndTS  >= ldPeriodFrom,
      FIRST bfmServiceLimit NO-LOCK WHERE
            bfmServiceLimit.SlSeq = bfmServiceLCounter.SlSeq AND
            bfmServiceLimit.GroupCode = icServiceLimit:
 
      /* % of fee, based on usage */
      ldFeeAmount = fCalculateProportionalFee(bfmServiceLCounter.Amt,
                                              bfmMServiceLimit.InclUnit,
                                              bfmMServiceLimit.InclAmt,
                                              idAmount).
   END.

   RETURN ldFeeAmount. 

END FUNCTION.

FUNCTION fCalculatePrepaidFirstMonthFAT RETURNS DEC
   (icCLI          AS CHAR, 
    iiMsSeq        AS INT, 
    idAmount       AS DEC,  /* original amount (full month) */
    iiPeriod       AS INT): /* YYYYMM */

   DEF VAR ldFatAmount AS DEC  NO-UNDO.
   DEF VAR ldMonthlyFee AS DEC NO-UNDO.
   DEF VAR liDataConsumption AS INT NO-UNDO.
   DEF VAR ldaPeriodFrom AS DATE NO-UNDO.
   DEF VAR ldaPeriodTo AS DATE NO-UNDO.
   DEF VAR liPos AS INT NO-UNDO. 
   DEF VAR lcFormat AS CHARACTER.
   
   DEF BUFFER PrepCDR FOR PrepCDR.
   DEF BUFFER MCDRDtl2 FOR MCDRDtl2.
   
   ASSIGN
      ldFatAmount = idAmount
      ldaPeriodFrom = DATE((iiPeriod mod 100), 1, INT(iiPeriod / 100))
      ldaPeriodTo = fLastDayOfMonth(ldaPeriodFrom).

   FIND LAST PrepCDR NO-LOCK WHERE
             PrepCDR.CLI = icCLI AND
             PrepCDR.MsSeq = iiMsSeq AND
             PrepCDR.ErrorCode = 0 AND
             PrepCDR.DateSt >= ldaPeriodFrom AND
             PrepCDR.DateSt <= ldaPeriodTo AND
             PrepCDR.EventType = "GPRS" AND
             PrepCDR.MSCID NE "CCGW" USE-INDEX CLI NO-ERROR.

   IF AVAIL PrepCDR THEN DO:

      FIND FIRST McdrDtl2 WHERE
                 McdrDtl2.DateSt = PrepCDR.DateST AND
                 McdrDtl2.DtlSeq = PrepCDR.DtlSeq NO-LOCK NO-ERROR.
      
      IF AVAIL McdrDtl2 THEN DO:
         
         lcFormat = TRIM(ENTRY(4, mcdrdtl2.detail, "|")) NO-ERROR.
         IF ERROR-STATUS:ERROR THEN RETURN ldFatAmount.
         
         liPos = fGetPosition(McdrDtl2.Version + lcFormat,
                              "Accumulator value").
         IF liPos > 0 THEN DO:
            liDataConsumption = INT(REPLACE(ENTRY(liPos, MCDRDtl2.Detail, "|"),
                                    ",", ".")) NO-ERROR.
            IF liDataConsumption > 0 THEN DO:
               ldFatAmount = idAmount -
                              (liDataConsumption / (500 * 1024)) * idAmount.
               IF ldFatAmount < 0 THEN ldFatAmount = 0.
            END.
         END.
      END.
   END.      

   RETURN ldFatAmount.

END FUNCTION.

&ENDIF
