/* ----------------------------------------------------------------------
  module .......: Mm/q25_reminder.p
  task .........: Notify customer for Q25 ending
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

/* Handling of sms sending is different at January 2016 */
IF DAY(TODAY) > 15 OR TODAY < 1/13/16 THEN
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
   /* works when executed during 1. - 15. of each month as planned */
   liStartDay = (DAY(TODAY) * 2) - 1. 
   liEndDay = (DAY(TODAY) * 2).
END.

/* Month 22 */
IF fCheckDates(2, liStartDay, liEndDay, ldaStartDate, ldaEndDate) THEN DO:
   fSendQ25SMSMessages(ldaStartDate, ldaEndDate, 1).
END.

/* Month 23 */
IF fCheckDates(1, liStartDay, liEndDay, ldaStartDate, ldaEndDate) THEN DO:
   fSendQ25SMSMessages(ldaStartDate, ldaEndDate, 2).
END.

/* Month 24 */
IF fCheckDates(0, liStartDay, liEndDay, ldaStartDate, ldaEndDate) THEN DO:
   fSendQ25SMSMessages(ldaStartDate, ldaEndDate, 3).
END.
