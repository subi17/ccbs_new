/* ----------------------------------------------------------------------
  module .......: Mm/q25_final_message.p
  task .........: Send info about Q25 payment. To be executed 21st day
                  of each month.
  application ..: tms
  author .......: kaaikas
  created ......: 19.11.15
  version ......: yoigo
---------------------------------------------------------------------- */

{commpaa.i}
ASSIGN gcBrand = "1"
       katun   = "CRON".
{q25functions.i}

DEF VAR liStartDay        AS INT  NO-UNDO.
DEF VAR liEndDay          AS INT  NO-UNDO.
DEF VAR liMonth           AS INT  NO-UNDO.
DEF VAR ldaStartDate      AS DATE NO-UNDO.
DEF VAR ldaEndDate        AS DATE NO-UNDO.


/* each month as planned */
   liStartDay = 1. /* First day of month */ 
   liEndDay = 30. /* Last day of month, special cases handled in fCheckDates */

/* Month 24 21st day*/
IF (DAY(TODAY) = 21) AND fCheckDates(0, liStartDay, liEndDay, ldaStartDate, ldaEndDate) THEN DO:
   fSendQ25SMSMessages(ldaStartDate, ldaEndDate, 4).
END.
