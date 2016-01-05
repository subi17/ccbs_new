/* ----------------------------------------------------------------------
  module .......: Mm/q25_reminder_collection.p
  task .........: Collect customer that needs to notify customer about
                  closing Quota 25 period ending and send SMSs.
  application ..: tms
  author .......: kaaikas
  created ......: 11.11.15
  version ......: yoigo
---------------------------------------------------------------------- */


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
DEF VAR liTempCount              AS INT  NO-UNDO.
DEF VAR lcLogText                AS CHAR NO-UNDO.
DEF VAR liTestStartDay           AS CHAR NO-UNDO.
DEF VAR liTestEndDay             AS CHAR NO-UNDO.
DEF VAR ldaExecuteDate           AS DATE NO-UNDO.

liQ25Logging = fCParamI("Q25LoggingLevel"). /* 0 = none, 1 = count, 2 = all */
lcExecuteDate = fCParam("Q25","Q25TestExecDate"). /* manipulated exec date */

/* For testing usage possibility to manipulate execution date. In actual 
   use parameter should be empty, so ELSE branch (TODAY) value is used. */
IF lcExecuteDate > "" THEN
   ldaExecuteDate = DATE(lcExecuteDate).
ELSE
   ldaExecuteDate = TODAY.

/* January 2016 messages will be sent during 20.1. - 30.1. after that this 
   can be removed because later on messages will be send between 1st and
   15th day of month. */
IF ldaExecuteDate < 1/31/16 THEN DO:
   liStartDay = ((DAY(ldaExecuteDate) - 19) * 3) - 2.
   liEndDay = ((DAY(ldaExecuteDate) - 19) * 3).
END.
ELSE IF DAY(ldaExecuteDate) > 15 THEN
   RETURN. /* All messages already send for this month */
ELSE DO:
   /* Other months collection is made during between 1st and 15th day of
    month. Handled two days cases in each of these days. At 1st contracts
    with validto date 1 and 2, 2nd day valid to dates 3 and 4 and so on. 
    15th day will be handled days 29-31. fCheckDates function resolves 
    last day of month. */
   liStartDay = (DAY(ldaExecuteDate) * 2) - 1. 
   liEndDay = (DAY(ldaExecuteDate) * 2).
END.

/* Month 22, 2 months perm contract to go */
fGetStartEndDates({&Q25_MONTH_22}, liStartDay, liEndDay,
                  OUTPUT ldaStartDateMonth22, OUTPUT ldaEndDateMonth22).
/* Month 23 1 month perm contract to go */
fGetStartEndDates({&Q25_MONTH_23}, liStartDay, liEndDay,
                  OUTPUT ldaStartDateMonth23, OUTPUT ldaEndDateMonth23).
/* Month 24 0 month perm contract to go */
fGetStartEndDates({&Q25_MONTH_24}, liStartDay, liEndDay,
                  OUTPUT ldaStartDateMonth24, OUTPUT ldaEndDateMonth24).

/* TESTING SUPPORT */
IF ldaExecuteDate <= 1/19/16 THEN DO:

ASSIGN lcTestStartDay     = fCParam("Q25","Q25TestStart")
       lcTestEndDay     = fCParam("Q25","Q25TestEnd").
   
   IF lcExecuteDate = "" AND lcTestStartDay > "" AND lcTestEndDay > "" THEN DO:
      ASSIGN
         liStartDay          = DAY(DATE(lcTestStartDay))
         liEndDay            = DAY(DATE(lcTestEndDay))
         ldaStartDateMonth24 = DATE(lcTestStartDay)
         ldaEndDateMonth24   = DATE(lcTestEndDay)
         ldaStartDateMonth23 = ADD-INTERVAL(ldaStartDateMonth24, 1, 'months':U)
         ldaEndDateMonth23   = ADD-INTERVAL(ldaEndDateMonth24, 1, 'months':U)
         ldaStartDateMonth22 = ADD-INTERVAL(ldaStartDateMonth24, 2, 'months':U)
         ldaEndDateMonth22   = ADD-INTERVAL(ldaEndDateMonth24, 2, 'months':U).
   END.
   ELSE IF lcExecuteDate = "" THEN
      RETURN. /* No test dates set, so nothing to do */

END.
/* Check first how many SMS is needed to send today, with third param value
   FALSE no actual sending, just calculation and log generation for testing
   and checking purposes. */
liTotalCount = fGenerateQ25SMSMessages(ldaStartDateMonth22, 
                   ldaEndDateMonth22, {&Q25_MONTH_22}, FALSE,
                   INPUT-OUTPUT liTempCount) + 
               fGenerateQ25SMSMessages(ldaStartDateMonth23, 
                   ldaEndDateMonth23, {&Q25_MONTH_23}, FALSE,
                   INPUT-OUTPUT liTempCount) +
               fGenerateQ25SMSMessages(ldaStartDateMonth24, 
                   ldaEndDateMonth24, {&Q25_MONTH_24}, FALSE, 
                   INPUT-OUTPUT liTempCount).
liTempCount = liTotalCount. /* for logging purposes */

lcLogText = "START|" + STRING(liStartDay) + "|" + STRING(liEndDay) + "|" + 
            STRING(ldaStartDateMonth22) + "|" + 
            STRING(ldaEndDateMonth22) + "|" + 
            STRING(ldaStartDateMonth23) + "|" + 
            STRING(ldaEndDateMonth23) + "|" + 
            STRING(ldaStartDateMonth24) + "|" + 
            STRING(ldaEndDateMonth24).
fQ25LogWriting(lcLogText, {&Q25_LOGGING_DETAILED}).

/* Actual SMS creation and sending */
IF ldaStartDateMonth22 NE ? AND ldaEndDateMonth22 NE ? THEN
   fGenerateQ25SMSMessages(ldaStartDateMonth22, ldaEndDateMonth22, 
                          {&Q25_MONTH_22}, TRUE, INPUT-OUTPUT liTotalCount).

/* Month 23 1 month perm contract to go */
IF ldaStartDateMonth23 NE ? AND ldaEndDateMonth23 NE ? THEN
   fGenerateQ25SMSMessages(ldaStartDateMonth23, ldaEndDateMonth23, 
                          {&Q25_MONTH_23}, TRUE, INPUT-OUTPUT liTotalCount).

/* Month 24 0 month perm contract to go */
IF ldaStartDateMonth24 NE ? AND ldaEndDateMonth24 NE ? THEN
   fGenerateQ25SMSMessages(ldaStartDateMonth24, ldaEndDateMonth24, 
                          {&Q25_MONTH_24}, TRUE, INPUT-OUTPUT liTotalCount).
fQ25LogWriting("FINISH: " + STRING(liTempCount) + " messages sent. " +
               STRING(liTotalCount) + " messages left to send.",
               {&Q25_LOGGING_COUNTERS}).
