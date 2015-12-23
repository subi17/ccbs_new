/* ----------------------------------------------------------------------
  module .......: Mm/q25_final_msg_collection.p
  task .........: Collect customer that needs to notify customer about
                  final fee. YPR-2972
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

DEF VAR liStartDay        AS INT  NO-UNDO.
DEF VAR liEndDay          AS INT  NO-UNDO.
DEF VAR liTotalCount      AS INT  NO-UNDO.
DEF VAR liTempCount       AS INT NO-UNDO.
DEF VAR ldaStartDateMonth24 AS DATE NO-UNDO.
DEF VAR ldaEndDateMonth24   AS DATE NO-UNDO.

/* each month as planned */
ASSIGN
   liStartDay = 1 /* First day of month */
   liEndDay = 30. /* Last day of month, special cases handled in fCheckDates */

/* Month 24 21st day*/
IF (DAY(TODAY) = 21) AND fGetStartEndDates({&Q25_MONTH_24}, liStartDay, 
                                           liEndDay, OUTPUT ldaStartDateMonth24,
                                           OUTPUT ldaEndDateMonth24) THEN DO:
   liTotalCount = fGenerateQ25SMSMessages(ldaStartDateMonth24, 
                                         ldaEndDateMonth24, 
                                         {&Q25_MONTH_24_FINAL_MSG}, FALSE, 
                                         INPUT-OUTPUT liTempCount).
   fQ25LogWriting(STRING(fMakeTS()) + "Start final MESSAGE sending. " + 
                  STRING(liTotalCount) + " messages to be send.",
                  {&Q25_LOGGING_COUNTERS}).
   fGenerateQ25SMSMessages(ldaStartDateMonth24, ldaEndDateMonth24, 
                          {&Q25_MONTH_24_FINAL_MSG}, TRUE,
                          INPUT-OUTPUT liTotalCount).
   fQ25LogWriting("End final message sending. " + STRING(liTotalCount) +
                  " messages left.", {&Q25_LOGGING_COUNTERS}).
END.
