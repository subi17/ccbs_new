/* ----------------------------------------------------------------------
  module .......: Mm/q25_reminder_sender.p
  task .........: Send collected Q25 SMS mesages
  application ..: tms
  author .......: kaaikas
  created ......: 20.11.15
  version ......: yoigo
---------------------------------------------------------------------- */
/* Sending SMS messages. To be triggered by cron for each hour between
   10:00 and 21:00. All messages should be sent before 22:00. HPD is capable
   to send 2-5 messages per second. (5 if length is less than 160 characters) 
   */ 

{commpaa.i}
ASSIGN gcBrand = "1"
       katun   = "CRON".
/*{q25functions.i}*/
{fgettxt.i}
{smsmessage.i}
{tmsconst.i}
{aes_encrypt.i}

/* send collected Quota 25 messages to customers */
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
   ELSE IF Q25Messaging.Phase = {&Q25_MONTH_24_FINAL_MSG} THEN DO:
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

   IF Q25Messaging.Phase < {&Q25_MONTH_24_FINAL_MSG} THEN DO:
   /* Month 22-24 */
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
              {&SMS_TYPE_Q25}).
   lcSentCount = lcSentCount + 1.
   ASSIGN Q25Messaging.isSent = TRUE.
   IF (lcSentCount >= {&MAXQ25MESSAGESPERHOUR} AND TIME < 75600) THEN
      LEAVE. /* maximum amount of messages sent per hour. Last sending
                interval is 21:00-22:00, send all rest messages here if
                reached */
END.


