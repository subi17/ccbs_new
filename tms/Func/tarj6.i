/* ----------------------------------------------------------------------
  MODULE .......: tarj6.i
  TASK .........: TARJ6 subscription type related functions
  APPLICATION ..: TMS
  AUTHOR .......: anttis
  CREATED ......: 13.02.13
  Version ......: Yoigo
----------------------------------------------------------------------- */
&IF "{&TARJ6_I}" NE "YES"
&THEN
&GLOBAL-DEFINE RATE_ERROR_CODES_I YES

{commali.i}
{date.i}
{cparam2.i}

FUNCTION fGetTARJ6DataLimitAndCharges RETURNS INT
   (iiMsSeq AS INT,
    OUTPUT odeMonthlyLimit AS DEC,
    OUTPUT odeUpsellChargeMonth AS DEC,
    OUTPUT odeUpsellChargeDay AS DEC,
    OUTPUT odeDailyChargeMonth AS DEC,
    OUTPUT odeDailyChargeDay AS DEC).
   
   DEF BUFFER ServiceLimit FOR ServiceLimit.
   DEF BUFFER MServicelimit FOR MServiceLimit.
   DEF BUFFER PrepEDR FOR PrepEDR.

   DEF VAR ldaFrom AS DATE NO-UNDO. 
   DEF VAR ldaTo AS DATE NO-UNDO. 
   DEF VAR ldeDayFrom AS DEC NO-UNDO. 
   DEF VAR ldeDayTo AS DEC NO-UNDO. 
   DEF VAR ldeFrom AS DEC NO-UNDO. 
   DEF VAR ldeTo AS DEC NO-UNDO. 
   DEF VAR liCharges AS INT NO-UNDO. 
   DEF VAR ldeUpsellFee AS DEC NO-UNDO. 

   ASSIGN
      ldaFrom  = DATE(MONTH(TODAY),1,YEAR(TODAY))
      ldaTo    = fLastDayOfMonth(ldaFrom)
      ldeFrom  = fMake2Dt(ldaFrom, 0)
      ldeTo    = fMake2Dt(ldaTo, 86399)
      ldeDayFrom = fMake2Dt(TODAY, 0)
      ldeDayTo = fMake2Dt(TODAY, 86399)
      ldeUpsellFee = fCParamDe("TARJ_UPSELLFee").
      
   /* count upsells */
   FOR FIRST ServiceLimit NO-LOCK WHERE
             ServiceLimit.GroupCode = "TARJ_UPSELL",
        EACH MServiceLimit NO-LOCK WHERE
             MServiceLimit.MsSeq = iiMsSeq AND
             MServiceLimit.DialType = ServiceLimit.DialType AND
             MServiceLimit.SLseq = ServiceLimit.SLSeq AND
             MServiceLimit.EndTS >= ldeFrom AND
             MServiceLimit.EndTS <= ldeTo:
      ASSIGN
         odeMonthlyLimit = odeMonthlyLimit + MServiceLimit.InclAmt
         odeUpsellChargeMonth = odeUpsellChargeMonth + ldeUpsellFee.

      IF MServiceLimit.EndTS >= ldeDayFrom AND
         MServiceLimit.EndTs <= ldeDayTo THEN ASSIGN
         odeUpsellChargeDay = odeUpsellChargeDay + ldeUpsellFee.
   END.
   
   /* count monthly 35MB charges */
   FOR EACH PrepEDR NO-LOCK WHERE
            PrepEDR.MsSeq = iiMsSeq AND
            PrepEDR.DateSt >= ldaFrom AND
            PrepEDR.DateSt <= ldaTo:
      IF PrepEDR.ErrorCode = 0 AND
         PrepEDR.SuccessCode EQ 1 THEN ASSIGN
         liCharges = liCharges + 1
         odeDailyChargeMonth = odeDailyChargeMonth + PrepEDR.SubscriberFee
         odeDailyChargeDay = odeDailyChargeDay + PrepEDR.SubscriberFee
            WHEN PrepEDR.DateSt EQ TODAY.
   END.

   odeMonthlyLimit = odeMonthlyLimit + (liCharges * 35.0).
   
END.

FUNCTION fGetTARJ6MonthlyDataLimit RETURNS INT
   (iiMsSeq AS INT):
   
   DEF BUFFER PrepEDR FOR PrepEDR.

   DEF VAR ldaFrom AS DATE NO-UNDO. 
   DEF VAR ldaTo AS DATE NO-UNDO. 
   DEF VAR liCharges AS INT NO-UNDO. 
   DEF VAR liDataLimit AS INT NO-UNDO. 

   ASSIGN
      ldaFrom  = DATE(MONTH(TODAY),1,YEAR(TODAY))
      ldaTo    = fLastDayOfMonth(ldaFrom).
      
   /* count monthly 35MB charges */
   FOR EACH PrepEDR NO-LOCK WHERE
            PrepEDR.MsSeq = iiMsSeq AND
            PrepEDR.DateSt >= ldaFrom AND
            PrepEDR.DateSt <= ldaTo:
      IF PrepEDR.ErrorCode = 0 AND
         PrepEDR.SuccessCode EQ 1 THEN 
         liCharges = liCharges + 1.
   END.

   RETURN (liCharges * 35).
   
END.


&ENDIF
