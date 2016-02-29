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
{fduedate.i}

DEF VAR lcTestStartDay AS CHAR NO-UNDO.
DEF VAR lcTestEndDay AS CHAR NO-UNDO.
DEF VAR lcExecuteDate AS CHAR NO-UNDO.
DEF VAR liQ25Logging AS INT NO-UNDO.
DEF VAR lcQ25LogDir          AS CHAR NO-UNDO.
DEF VAR lcQ25SpoolDir        AS CHAR NO-UNDO.
DEF VAR lcQ25LogFile         AS CHAR NO-UNDO.
DEF VAR ldnewAmount AS DEC NO-UNDO.

DEF STREAM Sout.

ASSIGN liQ25Logging = fCParamI("Q25LoggingLevel") /* 0 = none, 1 = sent msg, 
                                                     2 = count, 3 = all */
       lcQ25LogDir     = fCParam("Q25","Q25ReminderLogDir")
       lcQ25SpoolDir   = fCParam("Q25","Q25ReminderLogSpoolDir").
                  
IF lcQ25LogDir = "" OR lcQ25LogDir = ? THEN lcQ25LogDir = "/tmp/".
IF lcQ25SpoolDir = "" OR lcQ25SpoolDir = ? THEN lcQ25SpoolDir = "/tmp/".

/* Q24 Messages are needed to send before 20th day of month. No sending weekend
   or national holiday. Check if there are such days left or do we have to
   sent rest of messages right now. 19th day is last possible sending date.
   */
FUNCTION fIsLastDayToSend RETURNS LOGICAL (INPUT idaDate AS DATE, INPUT iiLastDay AS INT):
   DO WHILE idaDate < DATE(MONTH(idaDate),iiLastDay,YEAR(idaDate)).
      IF fChkDueDate(idaDate + 1) = idaDate + 1 THEN
         RETURN FALSE. /* there is at least one sending day left */
      idaDate = idaDate + 1.
   END.
   RETURN TRUE. /* is last day for sending */
END.

/* Function for getting start and end dates, based on calculated start day and
   end day. Checks that end date is not bigger than last day of month. 
   Start day and end day defines period where Q25 subscriptions are searched.
   Month Q22 = execution date + 2 months
   Month Q23 = execution date + 1 months 
   Month Q24 = execution month 
*/
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
   /* Start date should not be bigger than 29. 31 day handled todether with
      days 29 and 30 */
   IF iiStartDay > DAY(fLastDayOfMonth(ldaCountDate)) OR
      iiStartDay > 29 THEN
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

/* Make URL encoding, because it was not supported yeat in 10.2b version */
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

/* Calculate count of normal week day (no weekend ot national holiday) from
   sixth day (sms sending start date) until specified date. This is used for
   solving which messages should be sent in specified date. Goal is sent two
   messages in each day weekday after 6th until all messages of month is sent.
   */
FUNCTION fCountNormalWeekday RETURNS INTEGER (INPUT idaDate AS DATE,
                                              INPUT iiStartDay AS INT):
   DEF VAR lcCount AS INT NO-UNDO.
   DEF VAR ldaTempDate AS DATE NO-UNDO.

   ldaTempDate = DATE(MONTH(idaDate), iiStartDay, YEAR(idaDate)).
   DO WHILE ldaTempDate <= idaDate:
      IF fChkDueDate(ldaTempDate) EQ ldaTempDate THEN
         lcCount = lcCount + 1.
      ldaTempDate = ldaTempdate + 1.
   END.

   RETURN lccount.
END.

/* Function for finding correct SMS message to be send. */
FUNCTION fgetQ25SMSMessage RETURNS CHARACTER (INPUT iiPhase AS INT,
                                              INPUT idaValidTo AS DATE,
                                              INPUT idAmount AS DEC,
                                              INPUT icCli AS CHAR):
   DEF VAR lcSMSMessage      AS CHAR NO-UNDO.
   DEF VAR ldReqStamp        AS DEC  NO-UNDO.
   DEF VAR lcEncryptedMSISDN AS CHAR NO-UNDO.
   DEF VAR lcPassPhrase      AS CHAR NO-UNDO.
   DEF VAR lcAmount          AS CHAR NO-UNDO.
   
   lcAmount = STRING(idAmount,"->>>>>>9.99").
   lcAmount = LEFT-TRIM(lcAmount).
   lcAmount = REPLACE(lcAmount,".",",").
   
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
      lcSMSMessage = fGetSMSTxt("Q25FinalFeeMsgNoDecision",
                                TODAY,
                                1,
                                OUTPUT ldReqStamp).
      lcSMSMessage = REPLACE(lcSMSMessage,"#PAYMENT", lcAmount).
   END.
   ELSE IF iiPhase = {&Q25_MONTH_24_CHOSEN} THEN DO:
   /* Q25 Month 24 20th day extension made */  
      lcSMSMessage = fGetSMSTxt("Q25FinalFeeMsgChosenExt",
                                TODAY,
                                1,
                                OUTPUT ldReqStamp).
      lcSMSMessage = REPLACE(lcSMSMessage,"#PAYMENT", lcAmount).
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

/* Logs for testing */
FUNCTION fQ25LogWriting RETURNS LOGICAL
   (INPUT iclogText AS CHAR,
    INPUT iiLogLevel AS INT,
    INPUT iilogPhase AS INT).
   DEF VAR lcQ25LogType AS CHAR NO-UNDO. 
   /* Requested customer log writings. YPR-3446 */
   IF iiLogLevel EQ {&Q25_LOGGING_CUST_LOGS} THEN DO:
      lcQ25LogFile = lcQ25SpoolDir + "events_" +
                     (REPLACE(STRING(fMakeTS()),".","_")) + ".cvt".
      OUTPUT STREAM Sout TO VALUE(lcQ25LogFile) APPEND.
      PUT STREAM Sout UNFORMATTED
         icLogText SKIP.
      OUTPUT STREAM Sout CLOSE. 
      RETURN TRUE.
   END.
   /* Own internal log writings */
   IF liQ25Logging >= iiLogLevel THEN DO:
      /* Make Monthly log file */
      IF iilogPhase LT {&Q25_MONTH_24_FINAL_MSG} THEN
         lcQ25LogType = "reminder".
      ELSE
         lcQ25LogType = "final".
      lcQ25LogFile = lcQ25SpoolDir + "Q25_sms_message_logs_" +
                  lcQ25logType +
                  STRING(YEAR(TODAY)) +
                  STRING(MONTH(TODAY),"99") +
                  STRING(DAY(TODAY),"99") + ".txt".
      OUTPUT STREAM Sout TO VALUE(lcQ25LogFile) APPEND.
      PUT STREAM Sout UNFORMATTED
         STRING(fMakeTS()) + "|" + icLogText SKIP.
      OUTPUT STREAM Sout CLOSE.
   END.
END.

/* Function to calculate dynamically pause value between message sending. 
   To ensure all messages will be sent before 22:00, for safety reason
   calculation is made to 21:45 so there will be time to send last messages
   for sure before 22:00 */
FUNCTION fCalculateMaxPauseValue RETURN INTEGER
   (INPUT iiToBeSend AS INT).
   DEF VAR ldEndTime AS DEC NO-UNDO.
   DEF VAR ldTimeLeft AS DEC NO-UNDO.
   ldEndTime = fHMS2TS(TODAY, "21:45:00").
   ldTimeLeft = (ldEndTime - fMakeTS()) * 100000.
   IF iiToBeSend = 0 THEN RETURN 0. /* no messages left, no pause needed and
                                       do not divide by zero */
   RETURN INT(TRUNC(ldTimeLeft / iiToBeSend,0)). 
END.

/* SMS message generating and sending for Q25. */
FUNCTION fGenerateQ25SMSMessages RETURNS INTEGER 
   (INPUT idaStartDate AS DATE,
    INPUT idaEndDate AS DATE,
    INPUT iiphase AS INT,
    INPUT iiExecType AS INT,
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
   DEF VAR ldAmount          AS DEC NO-UNDO.
   DEF VAR lcTemplateName    AS CHAR NO-UNDO.
   DEF VAR liLogType         AS INT NO-UNDO.  
 
   DEF BUFFER bDCCLI         FOR DCCLI.

   IF idaStartDate = ? OR idaEndDate = ? THEN
      RETURN 0.

   ASSIGN liPauseValue = fCParamI("Q25SmsPause").
   IF liPauseValue = ? THEN
      liPauseValue = 10.

   liPeriod = YEAR(idaStartDate) * 100 + MONTH(idaStartDate).

   /* Special case, if order is done at 1st day of month Q0, period ends
      last day of previous month Q24. Need to include it. 
      for example contract ends 29.2.2016, singlefee is in 201603 */
   IF DAY(idaStartDate) = 1 THEN
      idaStartDate = idaStartDate - 1.
   FOR EACH SingleFee USE-INDEX BillCode WHERE
            SingleFee.Brand       = gcBrand AND
            SingleFee.Billcode BEGINS "RVTERM" AND
            SingleFee.HostTable   = "Mobsub" AND
            SingleFee.SourceTable = "DCCLI" AND
            SingleFee.CalcObj     = "RVTERM" AND
            SingleFee.BillPeriod  = liPeriod NO-LOCK:

      IF SingleFee.OrderId <= 0 THEN NEXT.
      liPhase = iiPhase.
      ldAmount = SingleFee.amt.
      FIND FIRST Mobsub NO-LOCK WHERE
                 Mobsub.MsSeq = INT(SingleFee.KeyValue) AND
                 Mobsub.Paytype = FALSE NO-ERROR.
      IF NOT AVAIL Mobsub THEN DO:
         lcLogText = "Mobsub not found or prepaid: " +
                     STRING(liPhase) + "|" + STRING(SingleFee.KeyValue) 
                     + "|" + STRING(ldAmount).
            fQ25LogWriting(lcLogText, {&Q25_LOGGING_DETAILED}, liphase).
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
                        STRING(ldAmount).
            fQ25LogWriting(lcLogText, {&Q25_LOGGING_DETAILED}, liphase).
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
                        STRING(ldAmount).
            fQ25LogWriting(lcLogText, {&Q25_LOGGING_DETAILED}, liphase).
         NEXT.
      END.
      ELSE IF DCCLI.TermDate NE ? THEN DO:
         liNotSendCount = liNotSendCount + 1.
         lcLogText = "DCCLI Terminated: " +
                        STRING(liPhase) + "|" + STRING(DCCLI.CLI) + "|" +
                        STRING(DCCLI.MsSeq) + "|" +
                        STRING(ldAmount).
            fQ25LogWriting(lcLogText, {&Q25_LOGGING_DETAILED}, liphase).
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
                        STRING(ldAmount).
            fQ25LogWriting(lcLogText, {&Q25_LOGGING_DETAILED}, liphase).
            NEXT.
             
         END.
            
         FIND FIRST bDCCLI NO-LOCK WHERE
                    bDCCLI.Brand   EQ gcBrand AND
                    bDCCLI.DCEvent EQ "RVTERM12" AND
                    bDCCLI.MsSeq   EQ Mobsub.MsSeq AND
                    bDCCLI.ValidTo >= TODAY NO-ERROR.
         IF AVAIL bDCCLI THEN DO:
            /* Q25 Extension already active */
            IF liPhase < {&Q25_MONTH_24_FINAL_MSG} THEN DO: 
            /* Q25 month 22-24 */
               /* before 21st day of month 24, no message needed for
                  customers who have already chosen quota 25 extension */
               liQ25DoneCount = liQ25DoneCount + 1.
               lcLogText = "Q25 already done: " +
                        STRING(liPhase) + "|" + STRING(bDCCLI.CLI) + "|" +
                        STRING(bDCCLI.MsSeq) + "|" +
                        STRING(ldAmount).
            fQ25LogWriting(lcLogText, {&Q25_LOGGING_DETAILED}, liphase).
               NEXT.
            END.
            ELSE
               /* 21st day and customer have decided to take Quota 25
                  extension. Send message with final payment / 12. */
               liPhase = {&Q25_MONTH_24_CHOSEN}.
               FIND FIRST FixedFee WHERE 
                          FixedFee.Brand EQ gcBrand AND
                          FixedFee.HostTable EQ "MobSub" AND
                          FixedFee.KeyValue EQ STRING(bDCCLI.MsSeq) AND
                          FixedFee.SourceTable EQ "DCCLI" AND
                          FixedFee.SourceKey EQ STRING(bDCCLI.PerContractID)
                          NO-LOCK NO-ERROR.
               IF AVAIL FixedFee THEN
                  ldAmount = FixedFee.amt.
               
         END.

         ELSE IF CAN-FIND(FIRST Order NO-LOCK WHERE
                                Order.MsSeq = mobsub.msseq AND
                                Order.OrderType = {&ORDER_TYPE_RENEWAL} AND
                                Order.CrStamp > fHMS2TS(ldaMonth22Date,
                                                        "00:00:00")) THEN DO:
         /* Renewal / Renuvo done */
            liNotSendCount = liNotSendCount + 1.
            lcLogText = "Renewal done: " +
                        STRING(liPhase) + "|" + STRING(Mobsub.CLI) + "|" +
                        STRING(Mobsub.MsSeq) + "|" +
                        STRING(ldAmount).
            fQ25LogWriting(lcLogText, {&Q25_LOGGING_DETAILED}, liphase).
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
                        STRING(liPhase) + "|" + STRING(Mobsub.CLI) + "|" +
                        STRING(Mobsub.MsSeq) + "|" +
                        STRING(ldAmount).
            fQ25LogWriting(lcLogText, {&Q25_LOGGING_DETAILED}, liphase).
            NEXT.
         END.
                                 
      END.
      liCount = liCount + 1. /* Full q25 count in Month */
   
      IF(iiExecType EQ {&Q25_EXEC_TYPE_SMS_SENDING}) THEN DO:
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
                        STRING(ldAmount).         
            fQ25LogWriting(lcLogText, {&Q25_LOGGING_DETAILED}, liphase).
            NEXT.
         END.
         ELSE DO:
            lcSMSMessage = fgetQ25SMSMessage(liphase, DCCLI.ValidTo + 1, 
                                             ldAmount, DCCLI.CLI).
            /* Send SMS */
            fCreateSMS(SingleFee.CustNum,
                       DCCLI.Cli,
                       DCCLI.MsSeq,
                       SingleFee.OrderId,
                       lcSMSMessage,
                       "622",
                       {&SMS_TYPE_Q25}).
            liSentCount = liSentCount + 1.
            lcLogText = STRING(liphase) + "|" + STRING(DCCLI.CLI) + "|" +
                        STRING(DCCLI.MsSeq).
            fQ25LogWriting(lcLogText, {&Q25_LOGGING_SENT_MSGS}, liphase).
            PAUSE liPauseValue.
            /* Decrease pause time if needed, check after each 50 sent SMS */
            IF (oiTotalCountLeft MODULO 50 = 0) AND 
                oiTotalCountLeft > 0 THEN DO:
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
                        STRING(DCCLI.MsSeq).
         END.
         ELSE DO:
            IF liPhase EQ {&Q25_MONTH_22} OR liPhase EQ {&Q25_MONTH_23} THEN
               lcTemplateName = "Q25ReminderMonth22and23".
            ELSE IF liPhase EQ {&Q25_MONTH_24} THEN
               lcTemplateName = "Q25ReminderMonth24".
            ELSE IF liPhase EQ {&Q25_MONTH_24_FINAL_MSG} THEN
               lcTemplateName = "Q25FinalFeeMsgNoDecision".
            ELSE
               lcTemplateName = "Error: Q25_Phase".
            IF (iiExecType EQ {&Q25_EXEC_TYPE_CUST_LOG_GENERATION}) AND
               lcTemplateName BEGINS "Q25" THEN DO:
               lcLogText = DCCLI.CLI + ";" + STRING(TODAY) + ";" +
                           lcTemplateName.
               liLogType = {&Q25_LOGGING_CUST_LOGS}.
            END.
            ELSE
               liLogType = {&Q25_LOGGING_DETAILED}.
               lcLogText = "Send SMS: " +
                           STRING(liPhase) + "|" + STRING(DCCLI.CLI) + "|" +
                           STRING(DCCLI.MsSeq) + "|" +
                           STRING(ldAmount) + "|" + lcTemplateName.
         END.   
         fQ25LogWriting(lcLogText, liLogType, liphase).
      END.
   END.
   /* Logging about amount of situations for testting purposes. */
   /* If ilSendMsgs is False, logging of calculated values to be done */
   IF (iiExecType EQ {&Q25_EXEC_TYPE_CALCULATION}) THEN DO:
      lcLogText = STRING(iiPhase) + "|".
      IF idaStartDate NE ? THEN
         lcLogText = lcLogText + "S:" + STRING(idaStartDate) + "|".
      IF idaEndDate NE ? THEN
         lcLogText = lcLogText + "E:" + STRING(idaEnddate) + "|".
      lcLogText = lcLogText + STRING(liCount) + "|" + STRING(liNotSendCount) + 
         "|" + STRING(liBilledCount) + "|" + STRING(liNotDCCLICount) + "|" +
         STRING(liReturnedDevices) + "|" + STRING(liQ25DoneCount) + 
         "|" + STRING(liPendingReq) + "|" + STRING(etime / 1000).
      fQ25LogWriting(lcLogText, {&Q25_LOGGING_COUNTERS}, liphase).
      RETURN liCount.
   END.
   RETURN oiTotalCountLeft.
END FUNCTION.


FUNCTION fBankByBillCode RETURNS CHAR
   (icBillCode AS CHAR):
   CASE icBillCode:
      WHEN "RVTERM1EF" THEN RETURN "UNO-E".
      WHEN "RVTERMBSF" THEN RETURN "Sabadell".
      WHEN "RVTERMF" THEN RETURN "Yoigo".
   END CASE.
   RETURN "".
END.

