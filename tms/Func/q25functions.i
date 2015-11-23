/* ----------------------------------------------------------------------
  MODULE .......: q25smsfunctions.i
  TASK .........: Functions for handling q25 SMS messaging and checking
                  needed data
  APPLICATION ..: tms
  AUTHOR .......:
  VERSION.......: 
  CREATED ......: 19.11.15
  CHANGED ......: 
  ------------------------------------------------------------------------*/


/* Function to check that calculated days exist in used month (if month
   have 31 days, then 15th day should send SMS for days 29-31. February
   could contain 28 or 29 days and so on.) */
{commali.i}
{timestamp.i}
{cparam2.i}
{fgettxt.i}
{date.i}
{smsmessage.i}
{aes_encrypt.i}

DEF STREAM Sout.

FUNCTION fCheckDates RETURNS LOGICAL
   (INPUT  iiMonths AS INT,
    INPUT  iiStartDay AS INT,
    INPUT  iiEndDay AS INT,
    OUTPUT odaStartDate AS DATE,
    OUTPUT odaEndDate AS DATE).
   DEF VAR  ldaCountDate       AS DATE NO-UNDO.
   ldaCountDate = ADD-INTERVAL(TODAY, iiMonths, 'months':U).
   IF iiStartDay > DAY(fLastDayOfMonth(ldaCountDate)) THEN
      RETURN FALSE.
   ELSE IF iiEndDay > DAY(fLastDayOfMonth(ldaCountDate))
      THEN iiEndDay = DAY(fLastDayOfMonth(ldaCountDate)).
   ELSE IF iiEndDay = 30 and DAY(fLastDayOfMonth(ldaCountDate)) = 31 THEN
      iiEndDay = 31.
   odaStartDate = DATE(MONTH(ldaCountDate),iiStartDay,YEAR(ldaCountDate)).
   odaEndDate = DATE(MONTH(ldaCountDate),iiEndDay,YEAR(ldaCountDate)).
   RETURN TRUE.
END FUNCTION.

FUNCTION fCollectQ25SMSMessages RETURNS LOGICAL
   (INPUT idaStartDate AS DATE,
    INPUT idaEndDate AS DATE,
    INPUT iiphase AS INT).
   /* Data collection function for Q25. To be launched by cron execution
      on 1.-15. day of month at morning time at least one hour before 10:00 */
   DEF VAR lcLogDir          AS CHAR NO-UNDO.
   DEF VAR lcLogFile         AS CHAR NO-UNDO.
   DEF VAR lcSMSText         AS CHAR NO-UNDO.
   DEF VAR liCount           AS INT  NO-UNDO.   
   DEF VAR lcPeriod          AS CHAR NO-UNDO.
   DEF VAR liNotSendCount    AS INT NO-UNDO.
   DEF VAR liBilledCount     AS INT NO-UNDO.
   DEF VAR liNotDCCLICount   AS INT NO-UNDO.
   DEF VAR liReturnedDevices AS INT NO-UNDO.
   DEF VAR liQ25DoneCount    AS INT NO-UNDO.
   DEF VAR liAlreadyCreated  AS INT NO-UNDO.
   DEF VAR liTempMsSeq       AS INT NO-UNDO.
   DEF VAR ldaMonth22Date    AS DATE NO-UNDO.

   ASSIGN lcLogDir     = fCParam("Q25","Q25_reminder_LogDir").

   IF lcLogDir = "" OR lcLogDir = ? THEN lcLogDir = "/tmp/".

   lcLogFile = lcLogDir + "Q25_sms_message_logs_" +
               STRING(YEAR(TODAY)) +
               STRING(MONTH(TODAY),"99") +
               /* STRING(DAY(TODAY),"99") + */ ".txt".

   OUTPUT STREAM Sout TO VALUE(lcLogFile) APPEND.

   lcPeriod = STRING(YEAR(idaStartDate)) + (IF(MONTH(idaStartDate) < 10) THEN
              "0" ELSE "") + STRING(MONTH(idaStartDate)).
   FOR EACH SingleFee USE-INDEX BillCode WHERE
            SingleFee.Brand       = gcBrand AND
            SingleFee.Billcode BEGINS "RVTERM" AND
            SingleFee.HostTable   = "Mobsub" AND
            SingleFee.SourceTable = "DCCLI" AND
            SingleFee.CalcObj     = "RVTERM" AND
            SingleFee.BillPeriod  = INT(lcPeriod) NO-LOCK:
      IF SingleFee.Billed AND
         NOT CAN-FIND(FIRST Invoice NO-LOCK WHERE
                            Invoice.Invnum = SingleFee.InvNum aND
                            Invoice.InvType = 99) THEN DO:
         liBilledCount = liBilledCount + 1.
         NEXT. /* "Residual fee billed". */
      END.
      FIND FIRST DCCLI USE-INDEX PerContractId NO-LOCK WHERE
              DCCLI.PerContractId = INT(SingleFee.sourcekey) AND
              DCCLI.Brand   = gcBrand AND
              DCCLI.DCEvent BEGINS "PAYTERM" AND
              DCCLI.MsSeq   = INT(SingleFee.KeyValue) AND
              DCCLI.ValidTo > idaStartDate AND
              DCCLI.ValidTo < idaEndDate NO-ERROR.

      IF NOT AVAIL DCCLI THEN DO:
         /* No DCCLI for example between start and end date, singlefee is for
            whole month or no DCCLI for some error case (?). */
         liNotDCCLICount = liNotDCCLICount + 1.
         NEXT.
      END.
      ELSE IF DCCLI.TermDate NE ? OR
              DCCLI.RenewalDate > ldaMonth22Date THEN DO:
         liNotSendCount = liNotSendCount + 1.
         NEXT. /* terminated or renewal done during 22-24 month
                  SMS should not be send. */
      END.
      ELSE DO:
         ASSIGN
            liTempMsSeq = DCCLI.MsSeq /* stored for Quota 25 check */
            ldaMonth22Date = ADD-INTERVAL(DCCLI.ValidFrom, 22, 'months':U)
            ldaMonth22Date = DATE(MONTH(ldaMonth22Date),1,YEAR(ldaMonth22Date)). 
         FIND FIRST TermReturn WHERE
                    TermReturn.OrderId = SingleFee.OrderId AND
                    TermReturn.ReturnTS > fHMS2TS(DCCLI.ValidFrom, "0") 
                    NO-LOCK NO-ERROR.
         IF AVAIL TermReturn AND TermReturn.deviceScreen AND
                  TermReturn.deviceStart THEN DO:
            /* Accepted return of device */
            liReturnedDevices = liReturnedDevices + 1.
            NEXT.
         END.
         ELSE IF CAN-FIND(FIRST DCCLI NO-LOCK WHERE
                  DCCLI.Brand   EQ gcBrand AND
                  DCCLI.DCEvent EQ "RVTERM12" AND
                  DCCLI.MsSeq   EQ liTempMsSeq AND
                  DCCLI.ValidTo >= TODAY) THEN DO:
            /* Q25 Extension already active */
            IF iiPhase < 4 THEN DO: /* Q25 month 22-24 */
               /* before 21st day of month 24, no message needed for
                  customers who have already chosen quota 25 extension */
               liQ25DoneCount = liQ25DoneCount + 1.
               NEXT.
            END.
            ELSE
               /* 21st day and customer have decided to take Quota 25
                  extension. Send message with final payment / 12. */
               iiPhase = {&Q25_MONTH_24_CHOSEN}.
         END.
      END.
      liCount = liCount + 1. /* Full q25 count in Month */
   
      /* Create table for sending messages in the second phase started
         by separate cron execution for each hour 10:00 - 21:00 */
      FIND FIRST Q25Messaging WHERE Q25Messaging.msseq = DCCLI.MsSeq AND
                                    Q25Messaging.sendDate = TODAY NO-LOCK
                                    NO-ERROR.
      IF AVAIL Q25Messaging THEN DO:
         /* Something have went wrong, SMS sending for today already marked
            for this subscriber */
         liAlreadyCreated = liAlreadyCreated + 1.
      END.
      ELSE DO:
         CREATE Q25Messaging.
         ASSIGN
            Q25Messaging.phase = iiPhase
            Q25Messaging.sendDate = TODAY
            Q25Messaging.MSSeq = DCCLI.MsSeq
            Q25Messaging.CustNum = SingleFee.CustNum
            Q25Messaging.Cli = DCCLI.Cli
            Q25Messaging.OrderId = SingleFee.OrderId
            Q25Messaging.isSent = FALSE
            Q25Messaging.ValidTo = DCCLI.ValidTo
            Q25Messaging.Amt = SingleFee.Amt.
      END.
   END.
   /* Logging about amount of situations for testting purposes. */
   PUT STREAM Sout UNFORMATTED
      STRING(idaStartDate) + "|" STRING(idaEnddate) + "|" +
      STRING(liCount) + "|" + STRING(liNotSendCount) + "|" +
      STRING(liBilledCount) + "|" + STRING(liNotDCCLICount) + "|" +
      STRING(liReturnedDevices) + "|" + STRING(liQ25DoneCount) + "|" +
      STRING(liAlreadyCreated) + "|" + STRING(etime / 1000) SKIP.
   OUTPUT STREAM Sout CLOSE.
   RETURN TRUE.
END FUNCTION.

FUNCTION fSendQ25SMSMessages RETURNS LOGICAL ().
   DEF VAR lcSentCount AS INT NO-UNDO.
   DEF VAR lcSMSMessage      AS CHAR NO-UNDO.
   DEF VAR ldReqStamp        AS DEC  NO-UNDO.
   DEF VAR lcEncryptedMSISDN AS CHAR NO-UNDO.
   FOR EACH Q25Messaging WHERE Q25Messaging.issent = FALSE AND
                               Q25Messaging.sendDate = TODAY:
      IF Q25Messaging.Phase = {&Q25_MONTH_22} OR 
         Q25Messaging.Phase = {&Q25_MONTH_23} THEN DO: 
         /* Q25 reminder month 22 or 23 */
         lcSMSMessage = fGetSMSTxt("Q25ReminderMonth22and23",
                                 TODAY,
                                 1,
                                 OUTPUT ldReqStamp).
         lcSMSMessage = REPLACE(lcSMSMessage,"#DATE","20" + "/" +
                                STRING(MONTH(Q25Messaging.ValidTo))).
      END.
      ELSE IF Q25Messaging.Phase = {&Q25_MONTH_24} THEN DO: 
      /* Q25 reminder month 24 */
         lcSMSMessage = fGetSMSTxt("Q25ReminderMonth24",
                                   TODAY,
                                   1,
                                   OUTPUT ldReqStamp).
         lcSMSMessage = REPLACE(lcSMSMessage,"#DD","20").
      END.
      ELSE IF Q25Messaging.Phase = {&Q25_MONTH_24_NO_DECISION} THEN DO: 
      /* Q25 month 24 after 20th day no decision */
         lcSMSMessage = fGetSMSTxt("Q25FinalFeeMessageNoDecision",
                                   TODAY,
                                   1,
                                   OUTPUT ldReqStamp).
         lcSMSMessage = REPLACE(lcSMSMessage,"#PAYMENT", 
                        STRING(Q25Messaging.Amt)).
      END.
      ELSE IF Q25Messaging.Phase = {&Q25_MONTH_24_CHOSEN} THEN DO: 
      /* Q25 Month 24 20th day extension made */
         lcSMSMessage = fGetSMSTxt("Q25FinalFeeMessageChosenExt",
                                   TODAY,
                                   1,
                                   OUTPUT ldReqStamp).
         lcSMSMessage = REPLACE(lcSMSMessage,"#PAYMENT",
                                STRING(Q25Messaging.Amt / 12)).
      END.

      IF Q25Messaging.Phase < 4 THEN DO: /* Month 22-24 */
         /* Encrypted MSISDN added to messages sent during 22 to 24 month */
         lcEncryptedMSISDN = encrypt_data(Q25Messaging.Cli,
                             {&ENCRYPTION_METHOD}, {&Q25_PASSPHRASE}).
         lcSMSMessage = REPLACE(lcSMSMessage, "#MSISDN", lcEncryptedMSISDN).
      END.
   
      /* Send SMS */
      fCreateSMS(Q25Messaging.Custnum,
                 Q25Messaging.Cli,
                 Q25Messaging.MsSeq,
                 Q25Messaging.OrderId,
                 lcSMSMessage,
                 "622",
                 {&SMS_TYPE_OFFER}).
      lcSentCount = lcSentCount + 1.
      ASSIGN Q25Messaging.isSent = TRUE.
      IF (lcSentCount >= {&MAXQ25MESSAGESPERHOUR} AND TIME < 75600) THEN
         LEAVE. /* maximum amount of messages sent per hour. Last sending
                   interval is 21:00-22:00, send all rest messages here if
                   reached */
   END.

END FUNCTION.
