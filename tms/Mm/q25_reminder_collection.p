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
{ftransdir.i}

DEF VAR liStartDay               AS INT  NO-UNDO.
DEF VAR liEndDay                 AS INT  NO-UNDO.
DEF VAR liStartDay24M            AS INT  NO-UNDO.
DEF VAR liEndDay24M              AS INT  NO-UNDO.
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
DEF VAR liWeekdayCount           AS INT  NO-UNDO.

/* for testing and logging support */
ASSIGN lcTestStartDay = fCParam("Q25","Q25TestStart")
       lcTestEndDay   = fCParam("Q25","Q25TestEnd")
       liQ25Logging   = fCParamI("Q25LoggingLevel") /* 0 = none, 1 = count, 
                                                      2 = all */
       lcExecuteDate  = fCParam("Q25","Q25TestExecDate"). /* manipulated exec 
                                                            date */

/* For testing usage possibility to manipulate execution date. In actual 
   use parameter should be empty, so ELSE branch (TODAY) value is used. */
IF lcExecuteDate NE ? AND lcExecuteDate GT "" THEN
   ldaExecuteDate = DATE(lcExecuteDate).
ELSE
   ldaExecuteDate = TODAY.

execution:
DO:
   /* January 2016 messages will be sent during 20.1. - 30.1. after that this 
      can be removed because later on messages will be send between 1st and
      15th day of month. */
   IF ldaExecuteDate LE 2/5/16 THEN
         LEAVE execution.
/*   ELSE IF ldaExecuteDate GT 2/5/16 AND
      ldaExecuteDate LT 1/31/16 THEN DO:
      liStartDay = ((DAY(ldaExecuteDate) - 19) * 3) - 2.
      liEndDay = ((DAY(ldaExecuteDate) - 19) * 3).
   END. */
   /* No sending first 5 days of month, no sending at Saturday or Sunday. */
   ELSE IF DAY(ldaExecuteDate) LE 5 THEN
      LEAVE execution. /* Messages will be sent after 5th day of month */   
   ELSE IF fChkDueDate(ldaExecuteDate) NE ldaExecuteDate THEN
      LEAVE execution. /* no sending weekend and national holiday */
   ELSE DO:
      /* Other months collection is made during Q22 and Q23weekdays after 
         5th day of month. Handled two days cases in each of these days. 
         No message sending at weekend and national holidays. 
         At 1st valid weekday after 5th day contracts with validto date 1, 2, 
         2nd day valid to dates 3,4 and so on. 
         
         Q24 all messages are needed to send before 20th day. So sending three
         days messages an each weekday. If national holidays, last weekday
         before 20th need to send all rest of day messages.

         fCheckDates function resolves last day of month. */
       
       liWeekdayCount = fCountNormalWeekday(ldaExecuteDate).       
       /* sending days for Q22 and Q23 */
       liStartDay = (liWeekdayCount * 2) - 1. 
       liEndDay = (liWeekdaycount * 2).
       /* Sending days for Q24 */
       liStartDay24M = (liWeekdayCount * 3) - 2.
       liEndDay24M = (liWeekdaycount * 3).
   END.

   /* Month 22, 2 months perm contract to go */
   fGetStartEndDates({&Q25_MONTH_22}, liStartDay, liEndDay,
                     OUTPUT ldaStartDateMonth22, OUTPUT ldaEndDateMonth22).
   /* Month 23 1 month perm contract to go */
   fGetStartEndDates({&Q25_MONTH_23}, liStartDay, liEndDay,
                     OUTPUT ldaStartDateMonth23, OUTPUT ldaEndDateMonth23).
   /* Month 24 0 month perm contract to go */
   fGetStartEndDates({&Q25_MONTH_24}, liStartDay24M, liEndDay24M,
                     OUTPUT ldaStartDateMonth24, OUTPUT ldaEndDateMonth24).
   /* at month 24 all messages are needed to be send before 20th day.
      If there is national holidays during 6th and 20th day, might be
      needed to send some extra messages at the end. */
   IF fisLastDayToSend(ldaExecuteDate) THEN
      ldaEndDateMonth24 = DATE(MONTH(ldaEndDateMonth24),
                               DAY(fLastDayOfMonth(ldaEndDateMonth24)),
                               YEAR(ldaEndDateMonth24)).
   /* TESTING SUPPORT 
      Start and end date manipulation */
   IF ldaExecuteDate EQ TODAY AND lcTestStartDay NE ? AND 
                                  lcTestStartDay NE "" AND
                                  lcTestEndDay NE ? AND
                                  lcTestEndDay NE "" THEN DO:

         ASSIGN
            liStartDay          = DAY(DATE(lcTestStartDay))
            liEndDay            = DAY(DATE(lcTestEndDay))
            ldaStartDateMonth24 = DATE(lcTestStartDay)
            ldaEndDateMonth24   = DATE(lcTestEndDay)
            ldaStartDateMonth23 = ADD-INTERVAL(ldaStartDateMonth24, 1, 
                                               'months':U)
            ldaEndDateMonth23   = ADD-INTERVAL(ldaEndDateMonth24, 1, 
                                               'months':U)
            ldaStartDateMonth22 = ADD-INTERVAL(ldaStartDateMonth24, 2, 
                                               'months':U)
            ldaEndDateMonth22   = ADD-INTERVAL(ldaEndDateMonth24, 2, 
                                               'months':U).

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

   lcLogText = "START|" + STRING(liStartDay) + "|" + STRING(liEndDay) + "|".
   IF ldaStartDateMonth22 NE ? AND ldaEndDateMonth22 NE ? THEN
      lcLogText = lcLogtext + "22:" + STRING(ldaStartDateMonth22) + "|" + 
                  STRING(ldaEndDateMonth22) + "|".
   IF ldaStartDateMonth23 NE ? AND ldaEndDateMonth23 NE ? THEN
      lcLogText = lcLogtext + "23:" + STRING(ldaStartDateMonth23) + "|" + 
                  STRING(ldaEndDateMonth23) + "|".
   IF ldaStartDateMonth24 NE ? AND ldaEndDateMonth24 NE ? THEN
      lcLogText = lcLogtext + "24:" + STRING(ldaStartDateMonth24) + "|" + 
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
   fQ25LogWriting("FINISH: Total " + STRING(liTempCount) + " messages. " +
                  STRING(liTotalCount) + " messages left to send.",
                  {&Q25_LOGGING_COUNTERS}).
   IF lcSpoolDir NE lcLogDir AND lcLogFile > "" THEN
      fMove2TransDir(lcSpoolDir + lcLogFile, "", lcLogDir).
END.
