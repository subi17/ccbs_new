/* ----------------------------------------------------------------------
  MODULE .......: q25functions.i
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
FUNCTION fGetStartEndDates RETURNS LOGICAL
   (INPUT  iiMonth AS INT,
    INPUT  iiStartDay AS INT,
    INPUT  iiEndDay AS INT,
    OUTPUT odaStartDate AS DATE,
    OUTPUT odaEndDate AS DATE).
   DEF VAR  ldaCountDate       AS DATE NO-UNDO.
   ldaCountDate = ADD-INTERVAL(TODAY, iiMonth, 'months':U).
   IF iiStartDay > DAY(fLastDayOfMonth(ldaCountDate)) THEN
      RETURN FALSE.
   ELSE IF iiEndDay > DAY(fLastDayOfMonth(ldaCountDate))
      THEN iiEndDay = DAY(fLastDayOfMonth(ldaCountDate)).
   ELSE IF iiEndDay = 30 and DAY(fLastDayOfMonth(ldaCountDate)) = 31 THEN
      iiEndDay = 31. /* Month have 31 days */
   IF iiStartDay > 0 AND iiEndDay > 0 THEN DO:
      odaStartDate = DATE(MONTH(ldaCountDate),iiStartDay,
                                 YEAR(ldaCountDate)).
      odaEndDate = DATE(MONTH(ldaCountDate),iiEndDay,YEAR(ldaCountDate)).
   END.
   RETURN TRUE.
END.
/*
FUNCTION fGetDates RETURNS LOGICAL
   (INPUT  iiStartDay AS INT,
    INPUT  iiEndDay AS INT,
    OUTPUT odaStartDateMonth22 AS DATE,
    OUTPUT odaEndDateMonth22 AS DATE,
    OUTPUT odaStartDateMonth23 AS DATE,
    OUTPUT odaEndDateMonth23 AS DATE,
    OUTPUT odaStartDateMonth24 AS DATE,
    OUTPUT odaEndDateMonth24 AS DATE):
   DEF VAR  ldaCountDate       AS DATE NO-UNDO.
   DEF VAR  ldaStartDate AS DATE NO-UNDO.
   DEF VAR  ldaEndDate AS DATE NO-UNDO.
   /* Month 22 */
   IF fGetStartEndDates(2, iiStartDay, iiEndDay, ldaStartDate, ldaEndDate) THEN 
      ASSIGN odaStartDateMonth22 = ldaStartDate
             odaEndDateMonth22 = ldaEndDate.
   ELSE 
      ASSIGN odaStartDateMonth22 = ?
             odaEndDateMonth22 = ?.
   /* Month 23 */
   IF fGetStartEndDates(1, iiStartDay, iiEndDay, ldaStartDate, ldaEndDate) THEN
      ASSIGN odaStartDateMonth22 = ldaStartDate
             odaEndDateMonth22 = ldaEndDate.
   ELSE
      ASSIGN odaStartDateMonth22 = ?
             odaEndDateMonth22 = ?.
   /* Month 24 */
   IF fGetStartEndDates(0, iiStartDay, iiEndDay, ldaStartDate, ldaEndDate) THEN
      ASSIGN odaStartDateMonth22 = ldaStartDate
             odaEndDateMonth22 = ldaEndDate.
   ELSE
      ASSIGN odaStartDateMonth22 = ?
             odaEndDateMonth22 = ?.
   RETURN TRUE.
END FUNCTION.
*/
FUNCTION fgetQ25SMSMessage RETURNS CHARACTER (INPUT iiPhase AS INT,
                                              INPUT idaValidTo AS DATE,
                                              INPUT idAmount AS DEC,
                                              INPUT icCli AS CHAR):
   DEF VAR lcSMSMessage      AS CHAR NO-UNDO.
   DEF VAR ldReqStamp        AS DEC  NO-UNDO.
   DEF VAR lcEncryptedMSISDN AS CHAR NO-UNDO.
   IF iiPhase = {&Q25_MONTH_22} OR
      iiPhase = {&Q25_MONTH_23} THEN DO:
      /* Q25 reminder month 22 or 23 */
      lcSMSMessage = fGetSMSTxt("Q25ReminderMonth22and23",
                              TODAY,
                              1,
                              OUTPUT ldReqStamp).
      lcSMSMessage = REPLACE(lcSMSMessage,"#DATE","20" + "/" +
                             STRING(MONTH(idaValidTo))).
   END.
   ELSE IF iiPhase = {&Q25_MONTH_24} THEN DO:
   /* Q25 reminder month 24 */
      lcSMSMessage = fGetSMSTxt("Q25ReminderMonth24",
                                TODAY,
                                1,
                                OUTPUT ldReqStamp).
      lcSMSMessage = REPLACE(lcSMSMessage,"#DD","20").
   END.
   ELSE IF iiPhase = {&Q25_MONTH_24_FINAL_MSG} THEN DO:
   /* Q25 month 24 after 20th day no decision */
      lcSMSMessage = fGetSMSTxt("Q25FinalFeeMessageNoDecision",
                                TODAY,
                                1,
                                OUTPUT ldReqStamp).
      lcSMSMessage = REPLACE(lcSMSMessage,"#PAYMENT",
                     STRING(idAmount)).
   END.
   ELSE IF iiPhase = {&Q25_MONTH_24_CHOSEN} THEN DO:
   /* Q25 Month 24 20th day extension made */
      lcSMSMessage = fGetSMSTxt("Q25FinalFeeMessageChosenExt",
                                TODAY,
                                1,
                                OUTPUT ldReqStamp).
      lcSMSMessage = REPLACE(lcSMSMessage,"#PAYMENT",
                             STRING(ROUND(idAmount / 12,2))).
   END.
   IF iiPhase < {&Q25_MONTH_24_FINAL_MSG} THEN DO:
   /* Month 22-24 */
      /* Encrypted MSISDN added to messages sent during 22 to 24 month */
      lcEncryptedMSISDN = encrypt_data(icCli,
                          {&ENCRYPTION_METHOD}, {&Q25_PASSPHRASE}).
      lcSMSMessage = REPLACE(lcSMSMessage, "#MSISDN", lcEncryptedMSISDN).
   END.
   RETURN lcSMSMessage.
END FUNCTION.

FUNCTION fQ25LogWriting RETURNS LOGICAL
   (INPUT iclogText AS CHAR).
   DEF VAR lcLogDir          AS CHAR NO-UNDO.
   DEF VAR lcLogFile         AS CHAR NO-UNDO.

   ASSIGN lcLogDir     = fCParam("Q25","Q25_reminder_LogDir").
   IF lcLogDir = "" OR lcLogDir = ? THEN lcLogDir = "/tmp/".
   /* Make Monthly log file */
   lcLogFile = lcLogDir + "Q25_sms_message_logs_" +
               STRING(YEAR(TODAY)) +
               STRING(MONTH(TODAY),"99") +
               /* STRING(DAY(TODAY),"99") + */ ".txt".
   OUTPUT STREAM Sout TO VALUE(lcLogFile) APPEND.
   PUT STREAM Sout UNFORMATTED
      STRING(fMakeTS()) + " " + icLogText SKIP.
   OUTPUT STREAM Sout CLOSE.
END.

FUNCTION fCalculateMaxPauseValue RETURN INTEGER
   (INPUT iiToBeSend AS INT).
   DEF VAR ldEndTime AS DEC NO-UNDO.
   DEF VAR ldTimeLeft AS DEC NO-UNDO.
   ldEndTime = fHMS2TS(TODAY, "21:30:00").
   ldTimeLeft = (ldEndTime - fMakeTS()) * 100000.
   RETURN INT(ldTimeLeft / iiToBeSend). 
END.

FUNCTION fCollectQ25SMSMessages RETURNS INTEGER 
   (INPUT idaStartDate AS DATE,
    INPUT idaEndDate AS DATE,
    INPUT iiphase AS INT,
    INPUT ilsendMsgs AS LOGICAL,
    INPUT-OUTPUT oitotalCountLeft AS INT):
   /* Data collection function for Q25. To be launched by cron execution
      on 1.-15. day of month at morning time at least one hour before 10:00 */
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
   DEF VAR liTimeLeft        AS INT NO-UNDO.
   DEF VAR lcSMSMessage      AS CHAR NO-UNDO.
   DEF VAR liSentCount       AS INT NO-UNDO.
   DEF VAR lcLogText         AS CHAR NO-UNDO.
   DEF VAR liPauseValue      AS INT NO-UNDO.
   DEF VAR liCalcPauseValue  AS INT NO-UNDO.

   IF idaStartDate = ? OR idaEndDate = ? THEN
      RETURN 0.
   ASSIGN liPauseValue = fCParamI("Q25_sms_pause").
   IF liPauseValue = 0 OR liPauseValue = ? THEN
      liPauseValue = 10.

   lcPeriod = STRING(YEAR(idaStartDate)) + (IF(MONTH(idaStartDate) < 10) THEN
              "0" ELSE "") + STRING(MONTH(idaStartDate)).
   /* Special case, if order is one at 1st day of month Q1 period ends
      last day of previous month Q24. Need to include it. */
   IF (iiPhase = {&Q25_MONTH_24} OR iiPhase = {&Q25_MONTH_24_FINAL_MSG}) AND 
       DAY(idaStartDate) = 1 THEN
      idaStartDate = idaStartDate - 1.
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
              DCCLI.ValidTo >= idaStartDate AND
              DCCLI.ValidTo <= idaEndDate NO-ERROR.

      IF NOT AVAIL DCCLI THEN DO:
         /* No DCCLI for example between start and end date, singlefee is for
            whole month or no DCCLI for some error case (?). */
         liNotDCCLICount = liNotDCCLICount + 1.
         NEXT.
      END.
      ELSE IF DCCLI.TermDate NE ? THEN DO:
         liNotSendCount = liNotSendCount + 1.
         NEXT. /* terminated, SMS should not be send. */
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
            IF iiPhase < {&Q25_MONTH_24_FINAL_MSG} THEN DO: 
            /* Q25 month 22-24 */
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
         ELSE IF CAN-FIND(FIRST Order NO-LOCK WHERE
                                Order.MsSeq = liTempMsSeq AND
                                Order.OrderType = {&ORDER_TYPE_RENEWAL} AND
                                Order.CrStamp > fHMS2TS(ldaMonth22Date,
                                                        "00:00:00")) THEN DO:
         /* Renewal / Renuvo done */
            liNotSendCount = liNotSendCount + 1.
            NEXT.
         END.

         
      END.
      liCount = liCount + 1. /* Full q25 count in Month */
   
      /* Create table for sending messages in the second phase started
         by separate cron execution for each hour 10:00 - 21:00 */
      IF(ilSendMsgs) THEN DO:
         oiTotalCountLeft = oiTotalcountLeft - 1.
         FIND FIRST SMSMessage WHERE SMSMessage.msseq = DCCLI.MsSeq AND
                                     SMSMessage.CreStamp > fDate2TS(TODAY) AND
                                     SMSMessage.SMSType = {&SMS_TYPE_Q25}
                                     NO-LOCK NO-ERROR.
         
         IF AVAIL SMSMessage THEN DO:
            /* Something have went wrong, SMS sending for today already marked
               for this subscriber */
            liAlreadyCreated = liAlreadyCreated + 1.
         END.
         ELSE DO:
            lcSMSMessage = fgetQ25SMSMessage(iiphase, DCCLI.ValidTo, 
                                             SingleFee.amt, DCCLI.CLI).
            /* Send SMS */
            fCreateSMS(SingleFee.CustNum,
                       DCCLI.Cli,
                       DCCLI.MsSeq,
                       SingleFee.OrderId,
                       lcSMSMessage,
                       "622",
                       {&SMS_TYPE_Q25}).
            liSentCount = liSentCount + 1.
            PAUSE liPauseValue.
            /* Decrease pause time if needed, check after each 50 sent SMS */
            IF (oiTotalCountLeft MODULO 50 = 0) THEN DO:
               liCalcPauseValue = fCalculateMaxPauseValue(oiTotalCountLeft).
               IF (liCalcPauseValue < liPauseValue) THEN
                  liPauseValue = liCalcPauseValue. 
            END.
         END.
      END.   
   END.
   /* Logging about amount of situations for testting purposes. */
   /* If ilSendMsgs is False, logging of calculated values to be done */
   IF NOT(ilSendMsgs) THEN DO:
      lcLogText = STRING(iiPhase) + "|" +
                  STRING(idaStartDate) + "|" + STRING(idaEnddate) + "|" +
                  STRING(liCount) + "|" + STRING(liNotSendCount) + "|" +
                  STRING(liBilledCount) + "|" + STRING(liNotDCCLICount) + "|" +
                  STRING(liReturnedDevices) + "|" + STRING(liQ25DoneCount) + 
                  "|" +
                  STRING(liAlreadyCreated) + "|" + STRING(liSentCount) + "|" +
                  STRING(etime / 1000).
      fQ25LogWriting(lcLogText).
      RETURN liCount.
   END.
   RETURN oiTotalCountLeft.
END FUNCTION.

