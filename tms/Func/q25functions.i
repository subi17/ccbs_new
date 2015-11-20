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

FUNCTION fSendQ25SMSMessages RETURNS LOGICAL
   (INPUT idaStartDate AS DATE,
    INPUT idaEndDate AS DATE,
    INPUT iiphase AS INT).

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
   DEF VAR lcEncryptedMSISDN AS CHAR NO-UNDO.
   DEF VAR liTempMsSeq       AS INT NO-UNDO.
   DEF VAR ldaMonth22Date    AS DATE NO-UNDO.
   DEF VAR lcSMSMessage      AS CHAR NO-UNDO.
   DEF VAR ldReqStamp        AS DEC  NO-UNDO.

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
              DCCLI.ValidTo < idaStartDate AND
              DCCLI.ValidTo > idaEndDate NO-ERROR.
      ASSIGN
         liTempMsSeq = DCCLI.MsSeq /* stored for Quota 25 check */
         ldaMonth22Date = ADD-INTERVAL(DCCLI.ValidFrom, 22, 'months':U)
         ldaMonth22Date = DATE(MONTH(ldaMonth22Date),1,YEAR(ldaMonth22Date)).

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
         FIND FIRST TermReturn WHERE
                    TermReturn.OrderId = SingleFee.OrderId AND
                    TermReturn.ReturnTS > fHMS2TS(DCCLI.ValidFrom, "0").
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
            IF iiPhase < 4 THEN DO:
               /* before 21st day of month 24, no message needed for
                  customers who have already chosen quota 25 extension */
               liQ25DoneCount = liQ25DoneCount + 1.
               NEXT.
            END.
            ELSE
               /* 21st day and customer have decided to take Quota 25
                  extension. Send message with final payment / 12. */
               iiPhase = 5.
         END.
      END.
      liCount = liCount + 1. /* Full q25 count in Month */
      IF iiPhase = 1 OR iiPhase = 2 THEN DO: /* Q25 reminder month 22 or 23 */
         lcSMSMessage = fGetSMSTxt("Q25ReminderMonth22and23",
                                 TODAY,
                                 1, 
                                 OUTPUT ldReqStamp).
         lcSMSMessage = REPLACE(lcSMSMessage,"#DATE","20" + "/" +
                                         STRING(MONTH(DCCLI.ValidTo))).
      END.
      ELSE IF iiPhase = 3 THEN DO: /* Q25 reminder month 24 */
         lcSMSMessage = fGetSMSTxt("Q25ReminderMonth24",
                                 TODAY,
                                 1,
                                 OUTPUT ldReqStamp).
         lcSMSMessage = REPLACE(lcSMSMessage,"#DD","20").
      END. 
      ELSE IF iiPhase = 4 THEN DO: /* Q25 month 24 after 20th day no decision */
         lcSMSMessage = fGetSMSTxt("Q25FinalFeeMessageNoDecision",
                                 TODAY,
                                 1,
                                 OUTPUT ldReqStamp).
         lcSMSMessage = REPLACE(lcSMSMessage,"#PAYMENT", STRING(SingleFee.Amt)).
      END.
      ELSE IF iiPhase = 5 THEN DO: /* Q25 Month 24 20th day extension made */
         lcSMSMessage = fGetSMSTxt("Q25FinalFeeMessageChosenExt",
                                 TODAY,
                                 1,
                                 OUTPUT ldReqStamp).
         lcSMSMessage = REPLACE(lcSMSMessage,"#PAYMENT", 
                                STRING(SingleFee.Amt / 12)).
      END.

      IF iiPhase < 4 THEN DO:
         /* Encrypted MSISDN added to messages sent during 22 to 24 month */
         lcEncryptedMSISDN = encrypt_data(SingleFee.Cli,
                             {&ENCRYPTION_METHOD}, {&Q25_PASSPHRASE}).
         lcSMSMessage = REPLACE(lcSMSMessage, "#MSISDN", lcEncryptedMSISDN).
      END.

      /* Send SMS */
      fCreateSMS(SingleFee.Custnum,
                 SingleFee.Cli,
                 DCCLI.MsSeq,
                 SingleFee.OrderId,
                 lcSMSMessage,
                 "622",
                 {&SMS_TYPE_OFFER}).
   END.
   /* Logging about amount of situations for testting purposes. */
   PUT STREAM Sout UNFORMATTED
      STRING(idaStartDate) + "|" STRING(idaEnddate) + "|" +
      STRING(liCount) + "|" + STRING(liNotSendCount) + "|" +
      STRING(liBilledCount) + "|" + STRING(liNotDCCLICount) + "|" +
      STRING(liReturnedDevices) + "|" + STRING(liQ25DoneCount) + "|" +
      STRING(etime / 1000) SKIP.
   OUTPUT STREAM Sout CLOSE.
   RETURN TRUE.
END FUNCTION.
