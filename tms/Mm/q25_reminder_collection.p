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
IF fCheckDates(2, INPUT liStartDay, INPUT liEndDay, OUTPUT ldaStartDate, 
               OUTPUT ldaEndDate) THEN DO:
   fCollectQ25SMSMessages(ldaStartDate, ldaEndDate, {&Q25_MONTH_22}).
END.

/* Month 23 1 month perm contract to go */
IF fCheckDates(1, liStartDay, liEndDay, ldaStartDate, ldaEndDate) THEN DO:
   fCollectQ25SMSMessages(ldaStartDate, ldaEndDate, {&Q25_MONTH_23}).
END.

/* Month 24 0 month perm contract to go */
IF fCheckDates(0, liStartDay, liEndDay, ldaStartDate, ldaEndDate) THEN DO:
   fCollectQ25SMSMessages(ldaStartDate, ldaEndDate, {&Q25_MONTH_24}).
END.
