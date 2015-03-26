/* ----------------------------------------------------------------------
  Module .......: Func/contract_end_date.i
  Task .........: Exclude calculating the contract_end_date from
                  Mm/percontr.p and put it in separate function
  Application ..: TMS
  Author .......: Vikas
  Created ......: 03.10.11
  Version ......: Yoigo
---------------------------------------------------------------------- */

FUNCTION fcontract_end_date RETURNS DATE
   (INPUT  icDcEvent      AS CHARACTER,  /* DcEvent            */
    INPUT  idFromDate     AS DATE):      /* Contract From Date */

   DEFINE VARIABLE liYear        AS INTEGER  NO-UNDO.
   DEFINE VARIABLE liMonth       AS INTEGER  NO-UNDO.
   DEFINE VARIABLE liDate        AS INTEGER  NO-UNDO.
   DEFINE VARIABLE ldtEndDate    AS DATE     NO-UNDO.
   DEFINE VARIABLE ldtCountDate  AS DATE     NO-UNDO.

   DEFINE BUFFER b_DayCampaign   FOR DayCampaign.

   FIND FIRST b_DayCampaign WHERE
              b_DayCampaign.Brand   = gcBrand AND
              b_DayCampaign.dcEvent = icDcEvent NO-LOCK NO-ERROR.
   IF NOT AVAILABLE b_DayCampaign THEN DO:
      ldtEndDate = 12/31/2049.
      RETURN ldtEndDate.
   END. /* IF NOT AVAILABLE b_DayCampaign THEN DO: */

   /* predetermined length */
   IF (b_DayCampaign.DurType = 2 OR b_DayCampaign.DurType = 3) THEN DO:

      /* xx days */
      IF b_DayCampaign.DurUnit = 1 THEN 
         ldtEndDate = idFromDate + b_DayCampaign.DurMonths.

      ELSE DO:
         /* xx billing periods (currently calendar months) */
         IF b_DayCampaign.DurUnit = 3 THEN DO:
            IF MONTH(idFromDate) = 12 
            THEN ldtCountDate = DATE(1,1,YEAR(idFromDate) + 1).
            ELSE ldtCountDate = DATE(MONTH(idFromDate) + 1,1,
                                     YEAR(idFromDate)).

            ASSIGN
               liYear  = TRUNC((b_DayCampaign.DurMonths - 1) / 12,0) 
               liMonth = (b_DayCampaign.DurMonths - 1) MOD 12 
               liYear  = YEAR(ldtCountDate) + liYear
               liMonth = MONTH(ldtCountDate) + liMonth
               liDate  = 1. 
         END. /* IF b_DayCampaign.DurUnit = 3 THEN DO: */
         /* xx months onwards */
         ELSE DO:
            ASSIGN 
               liYear  = TRUNC(b_DayCampaign.DurMonths / 12,0) 
               liMonth = b_DayCampaign.DurMonths MOD 12 
               liYear  = YEAR(idFromDate) + liYear
               liMonth = MONTH(idFromDate) + liMonth
               liDate  = DAY(idFromDate).
         END. /* ELSE DO: */
         IF liMonth > 12 THEN ASSIGN 
            liYear  = liYear + 1
            liMonth = liMonth - 12.
        
         IF liMonth = 2 AND liDate > 28
         THEN liDate = DAY(DATE(3,1,liYear) - 1).
         ELSE IF liDate = 31 AND liMonth < 12 
         THEN liDate = DAY(DATE(liMonth + 1,1,liYear) - 1).

         ldtEndDate = DATE(liMonth,liDate,liYear) - 1.
      END. /* ELSE DO: */
   END. /* IF b_DayCampaign.DurType = 2 OR b_DayCampaign.DurType = 3 THEN DO: */

   /* until further */
   ELSE ldtEndDate = 12/31/2049.

   RETURN ldtEndDate.

END FUNCTION. /* FUNCTION fcontract_end_date RETURNS DATE */

