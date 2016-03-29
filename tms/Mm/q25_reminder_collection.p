/* ----------------------------------------------------------------------
  module .......: Mm/q25_reminder_collection.p
  task .........: Collect customer that needs to notify customer about
                  closing Quota 25 period ending and send SMSs.
  application ..: tms
  author .......: kaaikas
  created ......: 11.11.15
  version ......: yoigo
---------------------------------------------------------------------- */


{Syst/commpaa.i}
ASSIGN gcBrand = "1"
       katun   = "CRON".
{Func/q25functions.i}
{Func/ftransdir.i}

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
DEF VAR liWeekdayCount           AS INT  NO-UNDO.
DEF VAR liSendingStartDay        AS INT  NO-UNDO.
DEF VAR liRunMode                AS INT  NO-UNDO.
DEF VAR liQ22Q23SendingStartDay  AS INT  NO-UNDO.
DEF VAR liQ22Q23SendingEndDay    AS INT  NO-UNDO.
DEF VAR liQ24SendingStartDay     AS INT  NO-UNDO.
DEF VAR liQ24SendingEndDay       AS INT  NO-UNDO.
DEF VAR liQ22Q23PerDayCount         AS INT  NO-UNDO.
DEF VAR liQ24PerDayCount            AS INT  NO-UNDO.


/* for testing and logging support */
ASSIGN lcTestStartDay = fCParam("Q25","Q25TestStart")
       lcTestEndDay   = fCParam("Q25","Q25TestEnd")
       liQ25Logging   = fCParamI("Q25LoggingLevel") /* 0 = none, 1 = count, 
                                                      2 = all */
       lcExecuteDate  = fCParam("Q25","Q25TestExecDate"). /* manipulated exec 
                                                            date */
       liQ22Q23SendingStartDay = fCParamI("Q2223SendStart").
       liQ22Q23SendingEndDay = fCParamI("Q2223SendEnd").
       liQ24SendingStartDay = fCParamI("Q24SendStart").
       liQ24SendingEndDay = fCParamI("Q24SendEnd").
       liQ22Q23PerDayCount = fCParamI("Q2223PerDayCount").
       liQ24PerDayCount = fCParamI("Q24PerDayCount").


liRunMode = INT(SESSION:PARAMETER). /* get crontab input parameter, if this is 
                                       logging run (0) for making log file or 
                                       actual SMS calculation/sending (1) */

/* For testing usage possibility to manipulate execution date. In actual 
   use parameter should be empty, so ELSE branch (TODAY) value is used. */
IF lcExecuteDate NE ? AND lcExecuteDate GT "" THEN
   ldaExecuteDate = DATE(lcExecuteDate).
ELSE
   ldaExecuteDate = TODAY.

execution:
DO:
   /* Message sending is going to start 7.3.2016. This can be deleted 
      if refactoring/corrections is made after SMS sending released 
      Q22 and Q23 messages are send during 15th - last day of the month, Q24 
      messages during 6th - 19th day*/
   IF ldaExecuteDate LE 3/6/16 THEN
         LEAVE execution.
   ELSE IF fChkDueDate(ldaExecuteDate) NE ldaExecuteDate THEN
      LEAVE execution. /* no sending weekend and national holiday */
   ELSE DO:
      /* Other months collection is made during Q22 and Q23 weekdays starting 
         at 16th day of month. Handled three days cases in each of these days. 
         No message sending at weekend and national holidays. 
         At 1st valid weekday after 15th day contracts with validto date 1, 2, 
         3 and 2nd day valid to dates 4, 5, 6 and so on. 
         
         Q24 all messages are needed to send before 20th day. So sending three
         days messages an each weekday. If national holidays, last weekday
         before 20th need to send all rest of day messages. Message sending 
         is done weekdays during 6th - 19th day of the month.

         fCheckDates function resolves last day of month. */
       
       /* sending days for Q22 and Q23 */
       IF DAY(ldaExecuteDate) GE liQ22Q23SendingStartDay AND
          DAY(ldaExecuteDate) LE liQ22Q23SendingEndDay THEN DO:
          liSendingStartDay = liQ22Q23SendingStartDay. 
          liWeekdayCount = fCountNormalWeekday(ldaExecuteDate, 
                                               liSendingStartDay).
          liStartDay = (liWeekdayCount * liQ22Q23PerDayCount) - 
                       (liQ22Q23PerDayCount - 1).
          liEndDay = (liWeekdaycount * liQ22Q23PerDayCount).
       END.
       
       /* Sending days for Q24 */
       IF DAY(ldaExecuteDate) GE liQ24SendingStartDay AND
          DAY(ldaExecuteDate) LE liQ24SendingEndDay THEN DO:
          liSendingStartDay = liQ24SendingStartDay.
          liWeekdayCount = fCountNormalWeekday(ldaExecuteDate, 
                                               liSendingStartDay).
          liStartDay24M = (liWeekdayCount * liQ24PerDayCount) - 
                          (liQ24PerDayCount - 1).
          liEndDay24M = (liWeekdaycount * liQ24PerDayCount).
       END.
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
   /* at month 22 and 23 messages are needed to sent during month. Last day is 
      last weekday of the month. */
   /* Check that if last day parameter is bigger than last day of month, use
      last day of the month */
   IF (liQ22Q23SendingEndDay > DAY(fLastDayOfMonth(ldaExecuteDate))) THEN
      liQ22Q23SendingEndDay = DAY(fLastDayOfMonth(ldaExecuteDate)).
   IF fisLastDayToSend(ldaExecuteDate, liQ22Q23SendingEndDay) THEN DO:
      ldaEndDateMonth22 = DATE(MONTH(ldaEndDateMonth22),
                               DAY(fLastDayOfMonth(ldaEndDateMonth22)),
                               YEAR(ldaEndDateMonth22)).
      ldaEndDateMonth23 = DATE(MONTH(ldaEndDateMonth23),
                               DAY(fLastDayOfMonth(ldaEndDateMonth23)),
                               YEAR(ldaEndDateMonth23)).
   END.
   /* at month 24 all messages are needed to be send before 20th day.
      If there is national holidays during 6th and 20th day, might be
      needed to send some extra messages at the end. 19th day is last. */
   IF fisLastDayToSend(ldaExecuteDate, liQ24SendingEndDay) THEN
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
   /* If lirunmode = 0 then make only customer log writing else continue and 
      Check first how many SMS is needed to send today, with third param value
      0 and 1 no actual sending, just calculation and log generation for testing
      and checking purposes. 
      Defined run Modes (from crontab parameter):
      &GLOBAL-DEFINE Q25_EXEC_TYPE_CUST_LOG_GENERATION 0
      &GLOBAL-DEFINE Q25_EXEC_TYPE_CALCULATION 1 */
   liTotalCount = fGenerateQ25SMSMessages(ldaStartDateMonth22, 
                      ldaEndDateMonth22, {&Q25_MONTH_22}, liRunMode,
                      INPUT-OUTPUT liTempCount) + 
                  fGenerateQ25SMSMessages(ldaStartDateMonth23, 
                      ldaEndDateMonth23, {&Q25_MONTH_23}, liRunMode,
                      INPUT-OUTPUT liTempCount) +
                  fGenerateQ25SMSMessages(ldaStartDateMonth24, 
                      ldaEndDateMonth24, {&Q25_MONTH_24}, liRunMode, 
                      INPUT-OUTPUT liTempCount).
   IF liRunmode EQ 0 THEN DO:
      fMove2TransDir(lcQ25DWHLogFile, "", lcQ25DWHLogDir).
      LEAVE execution. /* DWH logs created, can leave here */ 
   END.
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
   fQ25LogWriting(lcLogText, {&Q25_LOGGING_DETAILED}, 0, liRunMode).

   /* Actual SMS creation and sending done here. Month 22, 2 months left */
   IF ldaStartDateMonth22 NE ? AND ldaEndDateMonth22 NE ? THEN
      fGenerateQ25SMSMessages(ldaStartDateMonth22, ldaEndDateMonth22, 
                             {&Q25_MONTH_22}, {&Q25_EXEC_TYPE_SMS_SENDING}, 
                             INPUT-OUTPUT liTotalCount).

   /* Month 23, 1 month perm contract left */
   IF ldaStartDateMonth23 NE ? AND ldaEndDateMonth23 NE ? THEN
      fGenerateQ25SMSMessages(ldaStartDateMonth23, ldaEndDateMonth23, 
                             {&Q25_MONTH_23}, {&Q25_EXEC_TYPE_SMS_SENDING}, 
                             INPUT-OUTPUT liTotalCount).

   /* Month 24, 0 month perm contract left */
   IF ldaStartDateMonth24 NE ? AND ldaEndDateMonth24 NE ? THEN
      fGenerateQ25SMSMessages(ldaStartDateMonth24, ldaEndDateMonth24, 
                             {&Q25_MONTH_24}, {&Q25_EXEC_TYPE_SMS_SENDING}, 
                             INPUT-OUTPUT liTotalCount).
   fQ25LogWriting("FINISH: Total " + STRING(liTempCount) + " messages. " +
                  STRING(liTotalCount) + " messages left to send.",
                  {&Q25_LOGGING_COUNTERS}, 0, liRunMode).
   IF lcQ25SpoolDir NE lcQ25LogDir AND lcQ25LogFile > "" THEN
      fMove2TransDir(lcQ25LogFile, "", lcQ25LogDir).
END.
