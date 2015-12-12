/* ----------------------------------------------------------------------
  module .......: Mm/q25_reminder_collection.p
  task .........: Collect customer that needs to notify customer about
                  closing Quota 25 period ending.
  application ..: tms
  author .......: kaaikas
  created ......: 11.11.15
  version ......: yoigo
---------------------------------------------------------------------- */
/* To be executed at 21st day of each month at least an 15 minutes before
   10:00. (In tests execution tooked about 11 minutes.) For to be sure
   execution for example at 7:00 might be wise, so data would be ready at
   10:00 */


{commpaa.i}
ASSIGN gcBrand = "1"
       katun   = "CRON".
{q25functions.i}

DEF VAR liStartDay               AS INT  NO-UNDO.
DEF VAR liEndDay                 AS INT  NO-UNDO.
DEF VAR ldaStartDateMonth22      AS DATE NO-UNDO.
DEF VAR ldaEndDateMonth22        AS DATE NO-UNDO.
DEF VAR ldaStartDateMonth23      AS DATE NO-UNDO.
DEF VAR ldaEndDateMonth23        AS DATE NO-UNDO.
DEF VAR ldaStartDateMonth24      AS DATE NO-UNDO.
DEF VAR ldaEndDateMonth24        AS DATE NO-UNDO.
DEF VAR liTotalCount             AS INT  NO-UNDO.
DEF VAR lcLogText                AS CHAR NO-UNDO.

/* Handling of sms sending is different at January 2016 
   it starts at 13th day and ends 15th */
IF TODAY < 1/1/16 THEN DO: /* For testing purposes */
   liStartDay = 1.
   liEndDay = 30.
END.
ELSE IF DAY(TODAY) > 15 OR TODAY < 1/13/16 THEN
   RETURN.
ELSE IF TODAY = 1/13/16 THEN DO:
   liStartDay = 1.
   liEndDay = 10.
END.
ELSE IF TODAY = 1/14/16 THEN DO:
   liStartDay = 11.
   liEndDay = 20.
END.
ELSE IF TODAY = 1/15/16 THEN DO:
   liStartDay = 21.
   liEndDay = 30.
END.

ELSE DO:
   /* Other months collection is made during between 1st and 15th day of
    month. Handled two days cases in each of these days. At 1st contracts
    with validto date 1 and 2, 2nd day valid to dates 3 and 4 and so on. 
    15th day will be handled days 29-31. fCheckDates function resolves 
    last day of month. */
   liStartDay = (DAY(TODAY) * 2) - 1. 
   liEndDay = (DAY(TODAY) * 2).
END.

/* Month 22, 2 months perm contract to go */
fGetStartEndDates({&Q25_MONTH_22}, liStartDay, liEndDay, ldaStartDateMonth22, 
                  ldaEndDateMonth22).
/* Month 23 1 month perm contract to go */
fGetStartEndDates({&Q25_MONTH_23}, liStartDay, liEndDay, ldaStartDateMonth23, 
                  ldaEndDateMonth23).
/* Month 24 0 month perm contract to go */
fGetStartEndDates({&Q25_MONTH_23}, liStartDay, liEndDay, ldaStartDateMonth24, 
                  ldaEndDateMonth24).

/* Check first how many SMS is needed to send today, with third param value
   FALSE no actual sending, just calculation and log generation for testing
   and checking purposes. */
liTotalCount = fCollectQ25SMSMessages(ldaStartDateMonth22, 
                   ldaEndDateMonth22, {&Q25_MONTH_22}, FALSE, 0) + 
               fCollectQ25SMSMessages(ldaStartDateMonth23, 
                   ldaEndDateMonth23, {&Q25_MONTH_23}, FALSE, 0) +
               fCollectQ25SMSMessages(ldaStartDateMonth24, 
                   ldaEndDateMonth24, {&Q25_MONTH_24}, FALSE, 0).

lcLogText = "START|" + STRING(liStartDay) + "|" + STRING(liEndDay) + "|" + 
            STRING(ldaStartDateMonth22) + "|" + 
            STRING(ldaEndDateMonth22) + "|" + 
            STRING(ldaStartDateMonth23) + "|" + 
            STRING(ldaEndDateMonth23) + "|" + 
            STRING(ldaStartDateMonth24) + "|" + 
            STRING(ldaEndDateMonth24).
fQ25LogWriting(lcLogText).

/* Actual SMS creation and sending */
IF ldaStartDateMonth22 NE ? AND ldaEndDateMonth22 NE ? THEN
   fCollectQ25SMSMessages(ldaStartDateMonth22, ldaEndDateMonth22, 
                          {&Q25_MONTH_22}, TRUE, liTotalCount).

/* Month 23 1 month perm contract to go */
IF ldaStartDateMonth23 NE ? AND ldaEndDateMonth23 NE ? THEN
   fCollectQ25SMSMessages(ldaStartDateMonth23, ldaEndDateMonth23, 
                          {&Q25_MONTH_23}, TRUE, liTotalCount).

/* Month 24 0 month perm contract to go */
IF ldaStartDateMonth24 NE ? AND ldaEndDateMonth24 NE ? THEN
   fCollectQ25SMSMessages(ldaStartDateMonth24, ldaEndDateMonth24, 
                          {&Q25_MONTH_24}, TRUE, liTotalCount).
fQ25LogWriting("FINISH").
