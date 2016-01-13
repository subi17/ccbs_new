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

DEF VAR lcTestStartDay AS CHAR NO-UNDO.
DEF VAR lcTestEndDay AS CHAR NO-UNDO.
DEF VAR lcExecuteDate AS CHAR NO-UNDO.
DEF VAR liQ25Logging AS INT NO-UNDO.

DEF STREAM Sout.

liQ25Logging = fCParamI("Q25LoggingLevel"). /* 0 = none, 1 = count, 2 = all */

FUNCTION fGetStartEndDates RETURNS LOGICAL
   (INPUT  iiMonth AS INT,
    INPUT  iiStartDay AS INT,
    INPUT  iiEndDay AS INT,
    OUTPUT odaStartDate AS DATE,
    OUTPUT odaEndDate AS DATE).
   DEF VAR  ldaCountDate       AS DATE NO-UNDO.
   IF lcExecuteDate > "" THEN
      ldaCountDate = ADD-INTERVAL(DATE(lcExecuteDate), iiMonth, 'months':U).
   ELSE
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

FUNCTION fUrlEncode RETURNS CHARACTER
  (INPUT icValue AS CHARACTER,
   INPUT icEnctype AS CHARACTER) :
/****************************************************************************
Description: Encodes unsafe characters in a URL as per RFC 1738 section 2.2.
  <URL:http://ds.internic.net/rfc/rfc1738.txt>, 2.2
Input Parameters: Character string to encode, Encoding option where "query",
  "cookie", "default" or any specified string of characters are valid.
  In addition, all characters specified in the global variable lcUnSafe
  plus ASCII values 0 <= x <= 31 and 127 <= x <= 255 are considered unsafe.
Returns: Encoded string  (unkown value is returned as blank)
Variables: lcUnSafe, lcReserved
****************************************************************************/
  DEFINE VARIABLE hx          AS CHARACTER NO-UNDO INITIAL "0123456789ABCDEF":U.
  DEFINE VARIABLE encode-list AS CHARACTER NO-UNDO.
  DEFINE VARIABLE i           AS INTEGER   NO-UNDO.
  DEFINE VARIABLE c           AS INTEGER   NO-UNDO.
  DEFINE VARIABLE lcUnSafe AS CHAR NO-UNDO INIT "/,=".
  DEFINE VARIABLE lcReserved AS CHAR NO-UNDO.


  /* Dont bother with blank or unknown  */
  IF LENGTH(icValue) = 0 OR icValue = ? THEN
    RETURN "".

  /* What kind of encoding should be used? */
  CASE icEnctype:
    WHEN "query":U THEN              /* QUERY_STRING name=value parts */
      encode-list = lcUnSafe + lcReserved + "+":U.
    WHEN "cookie":U THEN             /* Persistent Cookies */
      encode-list = lcUnSafe + " ,~;":U.
    WHEN "default":U OR WHEN "" THEN /* Standard URL encoding */
      encode-list = lcUnSafe.
    OTHERWISE
      encode-list = lcUnSafe + icEnctype.   /* user specified ... */
  END CASE.

  /* Loop through entire input string */
  ASSIGN i = 0.
  DO WHILE TRUE:
    ASSIGN
      i = i + 1
      /* ASCII value of character using single byte codepage */
      c = ASC(SUBSTRING(icValue, i, 1, "RAW":U), "1252":U, "1252":U).
    IF c <= 31 OR c >= 127 OR INDEX(encode-list, CHR(c)) > 0 THEN DO:
      /* Replace character with %hh hexidecimal triplet */
      SUBSTRING(icValue, i, 1, "RAW":U) =
        "%":U +
        SUBSTRING(hx, INTEGER(TRUNCATE(c / 16, 0)) + 1, 1, "RAW":U) + /* high */
        SUBSTRING(hx, c MODULO 16 + 1, 1, "RAW":U).             /* low digit */
      ASSIGN i = i + 2.   /* skip over hex triplet just inserted */
    END.
    IF i = LENGTH(icValue,"RAW":U) THEN LEAVE.
  END.

  RETURN icValue.
END FUNCTION.  /* furl-encode */

FUNCTION fgetQ25SMSMessage RETURNS CHARACTER (INPUT iiPhase AS INT,
                                              INPUT idaValidTo AS DATE,
                                              INPUT idAmount AS DEC,
                                              INPUT icCli AS CHAR):
   DEF VAR lcSMSMessage      AS CHAR NO-UNDO.
   DEF VAR ldReqStamp        AS DEC  NO-UNDO.
   DEF VAR lcEncryptedMSISDN AS CHAR NO-UNDO.
   DEF VAR lcPassPhrase      AS CHAR NO-UNDO.
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
      lcPassPhrase = fCParam("Q25","Q25PassPhrase").
      IF lcPassPhrase = "" OR lcPassPhrase = ? THEN 
         lcPassPhrase = {&Q25_PASSPHRASE}.
      lcEncryptedMSISDN = encrypt_data(icCli,
                          {&ENCRYPTION_METHOD}, lcPassPhrase).
      /* convert some special characters to url encoding (at least '+' char
         could cause problems at later phases. */
      lcEncryptedMSISDN = fUrlEncode(lcEncryptedMSISDN, "query").
      lcSMSMessage = REPLACE(lcSMSMessage, "#MSISDN", lcEncryptedMSISDN).
   END.
   RETURN lcSMSMessage.
END FUNCTION.

FUNCTION fQ25LogWriting RETURNS LOGICAL
   (INPUT iclogText AS CHAR,
    INPUT iiLogLevel AS INT).
   DEF VAR lcLogDir          AS CHAR NO-UNDO.
   DEF VAR lcLogFile         AS CHAR NO-UNDO.
   IF liQ25Logging >= iiLogLevel THEN DO:
      ASSIGN lcLogDir     = fCParam("Q25","Q25ReminderLogDir").
      IF lcLogDir = "" OR lcLogDir = ? THEN lcLogDir = "/tmp/".
      /* Make Monthly log file */
      lcLogFile = lcLogDir + "Q25_sms_message_logs_" +
                  STRING(YEAR(TODAY)) +
                  STRING(MONTH(TODAY),"99") +
                  STRING(DAY(TODAY),"99") + ".txt".
      OUTPUT STREAM Sout TO VALUE(lcLogFile) APPEND.
      PUT STREAM Sout UNFORMATTED
         STRING(fMakeTS()) + " " + icLogText SKIP.
      OUTPUT STREAM Sout CLOSE.
   END.
END.

FUNCTION fCalculateMaxPauseValue RETURN INTEGER
   (INPUT iiToBeSend AS INT).
   DEF VAR ldEndTime AS DEC NO-UNDO.
   DEF VAR ldTimeLeft AS DEC NO-UNDO.
   ldEndTime = fHMS2TS(TODAY, "21:30:00").
   ldTimeLeft = (ldEndTime - fMakeTS()) * 100000.
   RETURN INT(ldTimeLeft / iiToBeSend). 
END.

FUNCTION fGenerateQ25SMSMessages RETURNS INTEGER 
   (INPUT idaStartDate AS DATE,
    INPUT idaEndDate AS DATE,
    INPUT iiphase AS INT,
    INPUT ilsendMsgs AS LOGICAL,
    INPUT-OUTPUT oitotalCountLeft AS INT):
   /* Data collection function for Q25. To be launched by cron execution
      on 1.-15. day of month at morning time at least one hour before 10:00 */
   DEF VAR liCount           AS INT  NO-UNDO.   
   DEF VAR liPeriod          AS INT NO-UNDO.
   DEF VAR liNotSendCount    AS INT NO-UNDO.
   DEF VAR liBilledCount     AS INT NO-UNDO.
   DEF VAR liNotDCCLICount   AS INT NO-UNDO.
   DEF VAR liReturnedDevices AS INT NO-UNDO.
   DEF VAR liQ25DoneCount    AS INT NO-UNDO.
   DEF VAR liAlreadyCreated  AS INT NO-UNDO.
   DEF VAR ldaMonth22Date    AS DATE NO-UNDO.
   DEF VAR lcSMSMessage      AS CHAR NO-UNDO.
   DEF VAR liSentCount       AS INT NO-UNDO.
   DEF VAR lcLogText         AS CHAR NO-UNDO.
   DEF VAR liPauseValue      AS INT NO-UNDO.
   DEF VAR liCalcPauseValue  AS INT NO-UNDO.
   DEF VAR liPhase           AS INT NO-UNDO.
   DEF VAR liPendingReq      AS INT NO-UNDO.

   IF idaStartDate = ? OR idaEndDate = ? THEN
      RETURN 0.

   ASSIGN liPauseValue = fCParamI("Q25SmsPause").
   IF liPauseValue = ? THEN
      liPauseValue = 10.

   liPeriod = YEAR(idaStartDate) * 100 + MONTH(idaStartDate).

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
            SingleFee.BillPeriod  = liPeriod NO-LOCK:

      IF NOT SingleFee.OrderId NE 0 THEN NEXT.
      liPhase = iiPhase.
      FIND FIRST Mobsub NO-LOCK WHERE
                 Mobsub.MsSeq = INT(SingleFee.KeyValue) NO-ERROR.
      IF NOT AVAIL Mobsub THEN DO:
         lcLogText = "Mobsub not found: " +
                     STRING(liPhase) + "|" + STRING(SingleFee.KeyValue) 
                     + "|" + STRING(SingleFee.amt).
            fQ25LogWriting(lcLogText, {&Q25_LOGGING_DETAILED}).
         NEXT.
      END.
      IF SingleFee.Billed AND
         NOT CAN-FIND(FIRST Invoice NO-LOCK WHERE
                            Invoice.Invnum = SingleFee.InvNum aND
                            Invoice.InvType = 99) THEN DO:
         liBilledCount = liBilledCount + 1.
         lcLogText = "Residual fee Billed: " +
                        STRING(liPhase) + "|" + STRING(Mobsub.CLI) + "|" +
                        STRING(Mobsub.MsSeq) + "|" +
                        STRING(SingleFee.amt).
            fQ25LogWriting(lcLogText, {&Q25_LOGGING_DETAILED}).
         NEXT. /* "Residual fee billed". */
      END.

      FIND FIRST DCCLI USE-INDEX PerContractId NO-LOCK WHERE
              DCCLI.PerContractId = INT(SingleFee.sourcekey) AND
              DCCLI.Brand   = gcBrand AND
              DCCLI.DCEvent BEGINS "PAYTERM" AND
              DCCLI.MsSeq   = Mobsub.MsSeq AND
              DCCLI.ValidTo >= idaStartDate AND
              DCCLI.ValidTo <= idaEndDate NO-ERROR.

      IF NOT AVAIL DCCLI THEN DO:
         /* No DCCLI for example between start and end date, singlefee is for
            whole month or no DCCLI for some error case (?). */
         liNotDCCLICount = liNotDCCLICount + 1.
         lcLogText = "NO DCCLI FOUND " +
                        STRING(liPhase) + "|" + STRING(Mobsub.CLI) + "|" +
                        STRING(Mobsub.MsSeq) + "|" +
                        STRING(SingleFee.amt).
            fQ25LogWriting(lcLogText, {&Q25_LOGGING_DETAILED}).
         NEXT.
      END.
      ELSE IF DCCLI.TermDate NE ? THEN DO:
         liNotSendCount = liNotSendCount + 1.
         lcLogText = "DCCLI Terminated: " +
                        STRING(liPhase) + "|" + STRING(DCCLI.CLI) + "|" +
                        STRING(DCCLI.MsSeq) + "|" +
                        STRING(SingleFee.amt).
            fQ25LogWriting(lcLogText, {&Q25_LOGGING_DETAILED}).
         NEXT. /* terminated, SMS should not be send. */
      END.
      ELSE DO:
         ASSIGN
            ldaMonth22Date = ADD-INTERVAL(DCCLI.ValidFrom, 22, 'months':U)
            ldaMonth22Date = DATE(MONTH(ldaMonth22Date),1,YEAR(ldaMonth22Date)). 
         FIND FIRST TermReturn WHERE
                    TermReturn.OrderId = SingleFee.OrderId NO-LOCK NO-ERROR.
      
         IF AVAIL TermReturn AND 
             ((TermReturn.DeviceScreen = TRUE AND 
               TermReturn.DeviceStart = TRUE) OR 
              (TermReturn.DeviceScreen = ? AND
               TermReturn.DeviceStart  = ?)) THEN DO:
            /* Accepted return of device */
            liReturnedDevices = liReturnedDevices + 1.
            lcLogText = "Device returned " +
                        STRING(liPhase) + "|" + STRING(DCCLI.CLI) + "|" +
                        STRING(DCCLI.MsSeq) + "|" +
                        STRING(SingleFee.amt).
            fQ25LogWriting(lcLogText, {&Q25_LOGGING_DETAILED}).
            NEXT.
         END.
         ELSE IF CAN-FIND(FIRST DCCLI NO-LOCK WHERE
                  DCCLI.Brand   EQ gcBrand AND
                  DCCLI.DCEvent EQ "RVTERM12" AND
                  DCCLI.MsSeq   EQ Mobsub.MsSeq AND
                  DCCLI.ValidTo >= TODAY) THEN DO:
            /* Q25 Extension already active */
            IF liPhase < {&Q25_MONTH_24_FINAL_MSG} THEN DO: 
            /* Q25 month 22-24 */
               /* before 21st day of month 24, no message needed for
                  customers who have already chosen quota 25 extension */
               liQ25DoneCount = liQ25DoneCount + 1.
               lcLogText = "Q25 already done: " +
                        STRING(liPhase) + "|" + STRING(DCCLI.CLI) + "|" +
                        STRING(DCCLI.MsSeq) + "|" +
                        STRING(SingleFee.amt).
            fQ25LogWriting(lcLogText, {&Q25_LOGGING_DETAILED}).
               NEXT.
            END.
            ELSE
               /* 21st day and customer have decided to take Quota 25
                  extension. Send message with final payment / 12. */
               liPhase = {&Q25_MONTH_24_CHOSEN}.
         END.

         ELSE IF CAN-FIND(FIRST Order NO-LOCK WHERE
                                Order.MsSeq = mobsub.msseq AND
                                Order.OrderType = {&ORDER_TYPE_RENEWAL} AND
                                Order.CrStamp > fHMS2TS(ldaMonth22Date,
                                                        "00:00:00")) THEN DO:
         /* Renewal / Renuvo done */
            liNotSendCount = liNotSendCount + 1.
            lcLogText = "Renewal done: " +
                        STRING(liPhase) + "|" + STRING(DCCLI.CLI) + "|" +
                        STRING(DCCLI.MsSeq) + "|" +
                        STRING(SingleFee.amt).
            fQ25LogWriting(lcLogText, {&Q25_LOGGING_DETAILED}).
            NEXT.
         END.
         ELSE IF CAN-FIND(FIRST MsRequest NO-LOCK WHERE 
                                MsRequest.msSeq = mobsub.msseq AND
                                MsRequest.reqtype = 
                                   {&REQTYPE_CONTRACT_ACTIVATION} AND
                                MsRequest.reqStatus < 
                                   {&REQUEST_STATUS_DONE} AND
                                MsRequest.ReqCParam3 = "RVTERM12") THEN DO:
            /* Pending/ongoing Q25 request */
            liPendingReq = liPendingReq + 1.
            lcLogText = "Pending Q25 Request: " +
                        STRING(liPhase) + "|" + STRING(DCCLI.CLI) + "|" +
                        STRING(DCCLI.MsSeq) + "|" +
                        STRING(SingleFee.amt).
            fQ25LogWriting(lcLogText, {&Q25_LOGGING_DETAILED}).
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
         
         IF AVAIL SMSMessage AND (lcTestStartDay = "" OR 
                                  lcTestStartDay = ?) THEN DO:
            /* Something have went wrong, SMS sending for today already marked
               for this subscriber. If teststartday is defined testing ongoing
               and SMS messaging allowed. */
            liAlreadyCreated = liAlreadyCreated + 1.
            lcLogText = "SMS Already created: " +
                        STRING(liPhase) + "|" + STRING(DCCLI.CLI) + "|" +
                        STRING(DCCLI.MsSeq) + "|" +
                        STRING(SingleFee.amt).         
            fQ25LogWriting(lcLogText, {&Q25_LOGGING_DETAILED}).
            NEXT.
         END.
         ELSE DO:
            lcSMSMessage = fgetQ25SMSMessage(liphase, DCCLI.ValidTo + 1, 
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
      ELSE DO:
         /* Some logging about SMSs to be send. */
         IF liPhase = {&Q25_MONTH_24_CHOSEN} THEN DO:
            /* Q25 Month 24 20th day extension made */
            lcLogText = "Send SMS Q25 Chosen: " +
                        STRING(liPhase) + "|" + STRING(DCCLI.CLI) + "|" + 
                        STRING(DCCLI.MsSeq) + "|" + 
                        STRING(ROUND(SingleFee.amt / 12,2)).
         END.
         ELSE DO:
            lcLogText = "Send SMS: " +
                        STRING(liPhase) + "|" + STRING(DCCLI.CLI) + "|" +
                        STRING(DCCLI.MsSeq) + "|" +
                        STRING(SingleFee.amt).
         END.   
         fQ25LogWriting(lcLogText, {&Q25_LOGGING_DETAILED}).
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
                  "|" + STRING(liPendingReq) + "|" +
                  STRING(etime / 1000).
      fQ25LogWriting(lcLogText, {&Q25_LOGGING_COUNTERS}).
      RETURN liCount.
   END.
   RETURN oiTotalCountLeft.
END FUNCTION.
