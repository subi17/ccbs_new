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
ASSIGN gcBrand = "1"
       katun   = "CRON".
{Func/q25functions.i}
{Func/ftransdir.i}

DEF VAR liStartDay        AS INT  NO-UNDO.
DEF VAR liEndDay          AS INT  NO-UNDO.
DEF VAR liTotalCount      AS INT  NO-UNDO.
DEF VAR liTempCount       AS INT NO-UNDO.
DEF VAR ldaStartDateMonth24 AS DATE NO-UNDO.
DEF VAR ldaEndDateMonth24   AS DATE NO-UNDO.
DEF VAR liSendDay         AS INT NO-UNDO.
DEF VAR ldaTempDate       AS DATE NO-UNDO.
DEF VAR ldaExecuteDate    AS DATE NO-UNDO.

/* each month as planned */
ASSIGN
   liStartDay = 1 /* First day of month */
   liEndDay = 30. /* Last day of month, special cases handled in fCheckDates */

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


/* Month 24 21st day*/
fQ25LogWriting("START FINAL MESSAGE SENDING", {&Q25_LOGGING_COUNTERS},
               {&Q25_MONTH_24_FINAL_MSG}).

liSendDay = 21.  /* First possible sending day if not weekend or national 
                    holiday. Sending is done at first possible normal weekday 
                    in each month. (if 21. is saturday, send messages on 23.)*/

ldaTempDate = DATE(MONTH(ldaExecuteDate),liSendDay,YEAR(ldaExecuteDate)).
ldaTempDate = fChkDueDate(ldaTempDate). /* find normal weekday */
liSendDay = DAY(ldaTempDate). /* Add found day number here */
                    
IF (DAY(ldaExecuteDate) = liSendDay) AND 
    fGetStartEndDates({&Q25_MONTH_24}, liStartDay, liEndDay, 
    OUTPUT ldaStartDateMonth24, OUTPUT ldaEndDateMonth24) THEN DO:
   liTotalCount = fGenerateQ25SMSMessages(ldaStartDateMonth24, 
                                         ldaEndDateMonth24, 
                                         {&Q25_MONTH_24_FINAL_MSG}, FALSE, 
                                         INPUT-OUTPUT liTempCount).
   fQ25LogWriting(STRING(fMakeTS()) + "Start final MESSAGE sending. " + 
                  STRING(liTotalCount) + " messages to be send.",
                  {&Q25_LOGGING_COUNTERS}, {&Q25_MONTH_24_FINAL_MSG}).
   fGenerateQ25SMSMessages(ldaStartDateMonth24, ldaEndDateMonth24, 
                          {&Q25_MONTH_24_FINAL_MSG}, TRUE,
                          INPUT-OUTPUT liTotalCount).
   fQ25LogWriting("END FINAL MESSAGE SENDING. " + STRING(liTotalCount) +
                  " messages left.", {&Q25_LOGGING_COUNTERS}, 
                  {&Q25_MONTH_24_FINAL_MSG}).
   IF lcQ25SpoolDir NE lcQ25LogDir AND lcQ25LogFile > "" THEN
      fMove2TransDir(lcQ25SpoolDir + lcQ25LogFile + "final", "", lcQ25LogDir).
END.
