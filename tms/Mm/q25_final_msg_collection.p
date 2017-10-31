/* ----------------------------------------------------------------------
  module .......: Mm/q25_final_msg_collection.p
  task .........: Collect customer that needs to notify customer about
                  final fee. YPR-2972
  application ..: tms
  author .......: kaaikas
  created ......: 11.11.15
  version ......: yoigo
---------------------------------------------------------------------- */
{Syst/commpaa.i}
ASSIGN Syst.CUICommon:gcBrand = "1"
       Syst.CUICommon:katun   = "CRON".
{Func/q25functions.i}
{Func/ftransdir.i}

DEF VAR liStartDay        AS INT  NO-UNDO.
DEF VAR liEndDay          AS INT  NO-UNDO.
DEF VAR liTotalCount      AS INT  NO-UNDO.
DEF VAR liTempCount       AS INT NO-UNDO.
DEF VAR ldaStartDateMonth24 AS DATE NO-UNDO.
DEF VAR ldaEndDateMonth24   AS DATE NO-UNDO.
DEF VAR ldaTempDate       AS DATE NO-UNDO.
DEF VAR liRunMode         AS INT NO-UNDO.
DEF VAR liFinalMsgSendDay AS INT NO-UNDO.

liRunMode = INT(SESSION:PARAMETER).  /* get crontab input parameter:
                                        (0) - logging run for making log file
                                        (1) - actual SMS calculation/sending 
                                        (2) - Q25 Push Notification */

/* each month as planned */
ASSIGN
   liStartDay = 1 /* First day of month */
   liEndDay = 30. /* Last day of month, special cases handled in fCheckDates */

/* for testing and logging support */
ASSIGN lcTestStartDay = fCParam("Q25","Q25TestStart")
       lcTestEndDay   = fCParam("Q25","Q25TestEnd")
       liQ25Logging   = fCParamI("Q25LoggingLevel") /* 0 = none, 1 = count,
                                                      2 = all */
       lcExecuteDate  = fCParam("Q25","Q25TestExecDate") /* manipulated exec
                                                            date */
       liFinalMsgSendDay = fCParamI("Q25FinalMsgSendDay").
/* For testing usage possibility to manipulate execution date. In actual
   use parameter should be empty, so ELSE branch (TODAY) value is used. */
IF lcExecuteDate NE ? AND lcExecuteDate GT "" THEN
   ldaExecuteDate = DATE(lcExecuteDate).
ELSE
   ldaExecuteDate = TODAY.


/* Month 24 21st day*/

/* First possible sending day if not weekend or national holiday. Sending is 
   done at first possible normal weekday in each month. (if 16. is saturday, 
   send messages on 18.) First sending day can be set by tmsparams. */

ldaTempDate = DATE(MONTH(ldaExecuteDate),liFinalMsgSendDay,
                   YEAR(ldaExecuteDate)).
ldaTempDate = fChkDueDate(ldaTempDate). /* find normal weekday */
liFinalMsgSendDay = DAY(ldaTempDate). /* Add found day number here */

Execution:
DO:
   /* Q25 Push Notification */
   IF liRunMode = 2 AND fGetStartEndDates({&Q25_MONTH_24}, 
                                          liStartDay, 
                                          liEndDay, 
                                          OUTPUT ldaStartDateMonth24, 
                                          OUTPUT ldaEndDateMonth24) THEN DO:
      fGenerateQ25SMSMessages(ldaStartDateMonth24, 
                              ldaEndDateMonth24, 
                              {&Q25_MONTH_24_FINAL_MSG}, 
                              {&Q25_EXEC_TYPE_PUSH_SENDING},
                              INPUT-OUTPUT liTotalCount).
   END.
   ELSE IF (DAY(ldaExecuteDate) = liFinalMsgSendDay) AND 
            fGetStartEndDates({&Q25_MONTH_24}, liStartDay, liEndDay, 
            OUTPUT ldaStartDateMonth24, OUTPUT ldaEndDateMonth24) THEN DO:
            /* Generate customer logs (liRunMode = 0) or calculate cases and some 
               internal logs (liRunMode = 1) */
      liTotalCount = fGenerateQ25SMSMessages(ldaStartDateMonth24, 
                                            ldaEndDateMonth24, 
                                            {&Q25_MONTH_24_FINAL_MSG}, liRunMode, 
                                            INPUT-OUTPUT liTempCount).
      IF liRunMode EQ 0 THEN DO:
         fMove2TransDir(lcQ25DWHLogFile, "", lcQ25DWHLogDir).
         LEAVE Execution. /* DWH logs created, no need to continue */
      END.
      fQ25LogWriting(STRING(Func.Common:mMakeTS()) + "Start final MESSAGE sending. " + 
                     STRING(liTotalCount) + " messages to be send.",
                     {&Q25_LOGGING_COUNTERS}, {&Q25_MONTH_24_FINAL_MSG},
                     liRunMode).
      /* Send actual SMS Messages */
      fGenerateQ25SMSMessages(ldaStartDateMonth24, ldaEndDateMonth24, 
                             {&Q25_MONTH_24_FINAL_MSG}, 
                             {&Q25_EXEC_TYPE_SMS_SENDING},
                             INPUT-OUTPUT liTotalCount).
      fQ25LogWriting("END FINAL MESSAGE SENDING. " + STRING(liTotalCount) +
                     " messages left.", {&Q25_LOGGING_COUNTERS}, 
                     {&Q25_MONTH_24_FINAL_MSG}, liRunMode).
      IF lcQ25SpoolDir NE lcQ25LogDir AND lcQ25LogFile > "" THEN
         fMove2TransDir(lcQ25LogFile,"",lcQ25LogDir).
   END.
END.
