/* ----------------------------------------------------------------------
  module .......: Mm/q25_reminder_collection.p
  task .........: Collect customer that needs to notify customer about
                  closing Quota 25 period ending.
  application ..: tms
  author .......: kaaikas
  created ......: 11.11.15
  version ......: yoigo
---------------------------------------------------------------------- */

{commpaa.i}
ASSIGN gcBrand = "1"
       katun   = "CRON".
{q25functions.i}

DEF VAR liStartDay        AS INT  NO-UNDO.
DEF VAR liEndDay          AS INT  NO-UNDO.
DEF VAR ldaStartDate      AS DATE NO-UNDO.
DEF VAR ldaEndDate        AS DATE NO-UNDO.


/* each month as planned */
   liStartDay = 1. /* First day of month */
   liEndDay = 30. /* Last day of month, special cases handled in fCheckDates */

/* Month 24 21st day*/
IF (DAY(TODAY) = 21) AND fCheckDates(0, INPUT liStartDay, INPUT liEndDay, 
                            OUTPUT ldaStartDate, OUTPUT ldaEndDate) THEN DO:
   fCollectQ25SMSMessages(ldaStartDate, ldaEndDate, {&Q25_MONTH_24_FINAL_MSG}).
END.
