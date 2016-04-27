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

{commali.i}
{timestamp.i}
{cparam2.i}
{fgettxt.i}
{date.i}
{smsmessage.i}
{aes_encrypt.i}
{fduedate.i}
{ftransdir.i}
{fmakemsreq.i}
{coinv.i}


DEF VAR lcTestStartDay AS CHAR NO-UNDO.
DEF VAR lcTestEndDay AS CHAR NO-UNDO.
DEF VAR lcExecuteDate AS CHAR NO-UNDO.
DEF VAR ldaExecuteDate AS DATE NO-UNDO.
DEF VAR liQ25Logging AS INT NO-UNDO.
DEF VAR lcQ25LogDir          AS CHAR NO-UNDO.
DEF VAR lcQ25SpoolDir        AS CHAR NO-UNDO.
DEF VAR lcQ25LogFile         AS CHAR NO-UNDO.
DEF VAR lcQ25DWHLogFile         AS CHAR NO-UNDO.
DEF VAR lcQ25DWHLogDir      AS CHAR NO-UNDO.
DEF VAR ldnewAmount AS DEC NO-UNDO.
DEF VAR liNotSendCount    AS INT NO-UNDO.
DEF VAR lcSendingEndTime AS CHAR NO-UNDO.
DEF VAR lclcHRLPOutDir AS CHAR NO-UNDO.
DEF VAR lcHRLPListInDir AS CHAR NO-UNDO.
DEF VAR lcHrlpRemRedirDir AS CHAR NO-UNDO.
DEF VAR lcHRLPLogDir AS CHAR NO-UNDO.
DEF VAR lcHRLPSpoolDir AS CHAR NO-UNDO.
DEF VAR lcHRLPInProcDir AS CHAR NO-UNDO.
DEF VAR lcHRLPOutDir AS CHAR NO-UNDO.
DEF VAR lcHRLPOutFile AS CHAR NO-UNDO.
DEF VAR lcHRLPLogFile AS CHAR NO-UNDO.
DEF VAR lcHRLPRemRedirDirDir AS CHAR NO-UNDO.
DEF VAR lcHRLPTestMSSeq AS CHAR NO-UNDO. /*List of numbers accepted in test*/
DEF VAR liHRLPTestLevel AS INT NO-UNDO.
DEF VAR lcLandingPageLink AS CHAR NO-UNDO.
DEF VAR lcPassPhrase AS CHAR NO-UNDO.

DEF STREAM Sout.
DEF STREAM SHRLP.

ASSIGN liQ25Logging = fCParamI("Q25LoggingLevel") /* 0 = none, 1 = sent msg, 
                                                     2 = count, 3 = all */
       lcQ25LogDir     = fCParam("Q25","Q25ReminderLogDir")
       lcQ25SpoolDir   = fCParam("Q25","Q25ReminderLogSpoolDir")
       lcQ25DWHLogDir  = fCParam("Q25","Q25DWHLogDir")
       lcSendingEndTime = fCParam("Q25","Q25SendingEndTime")
       lcPassPhrase = fCParam("Q25","Q25PassPhrase").

IF lcPassPhrase = "" OR lcPassPhrase = ? THEN lcPassPhrase = {&Q25_PASSPHRASE}.
IF lcQ25LogDir = "" OR lcQ25LogDir = ? THEN lcQ25LogDir = "/tmp/".
IF lcQ25SpoolDir = "" OR lcQ25SpoolDir = ? THEN lcQ25SpoolDir = "/tmp/".

lcQ25DWHLogFile = lcQ25SpoolDir + "events_" +
                  (REPLACE(STRING(fMakeTS()),".","_")) + ".csv".


/* Function to check if there is available weekdays for SMS sending after
   specified day.
   Q24 Messages are needed to send before 20th day of month. No sending weekend
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
      ASSIGN
         odaStartDate = DATE(MONTH(ldaCountDate),iiStartDay,
                                   YEAR(ldaCountDate))
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
   given day (sms sending start date) until specified date. This is used for
   solving which messages should be sent in specified date. Goal is sent three
   messages in each day weekday starting at 6th/16th until all messages of 
   month is sent.
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
                                              INPUT iiBillPeriod AS INT,
                                              INPUT idAmount AS DEC,
                                              INPUT icCli AS CHAR):
   DEF VAR lcSMSMessage      AS CHAR NO-UNDO.
   DEF VAR ldReqStamp        AS DEC  NO-UNDO.
   DEF VAR lcEncryptedMSISDN AS CHAR NO-UNDO.
   DEF VAR lcAmount          AS CHAR NO-UNDO.
   
   ASSIGN
      lcAmount = STRING(idAmount,"->>>>>>9.99")
      lcAmount = LEFT-TRIM(lcAmount)
      lcAmount = REPLACE(lcAmount,".",",").
   
   IF iiPhase = {&Q25_MONTH_22} OR
      iiPhase = {&Q25_MONTH_23} THEN DO:
      /* Q25 reminder month 22 or 23 */
      ASSIGN
         lcSMSMessage = fGetSMSTxt("Q25ReminderMonth22and23",
                                   TODAY, 1, OUTPUT ldReqStamp)
         lcSMSMessage = REPLACE(lcSMSMessage,"#DATE","20" + "/" +
                                STRING(iiBillPeriod MOD 100)).
   END.
   ELSE IF iiPhase = {&Q25_MONTH_24} THEN DO:
   /* Q25 reminder month 24 */
      ASSIGN
         lcSMSMessage = fGetSMSTxt("Q25ReminderMonth24",
                                   TODAY, 1, OUTPUT ldReqStamp)
         lcSMSMessage = REPLACE(lcSMSMessage,"#DD","20").
   END.
   ELSE IF iiPhase = {&Q25_MONTH_24_FINAL_MSG} THEN DO:
   /* Q25 month 24 after 20th day no decision */
      ASSIGN
         lcSMSMessage = fGetSMSTxt("Q25FinalFeeMsgNoDecision",
                                   TODAY, 1, OUTPUT ldReqStamp)
         lcSMSMessage = REPLACE(lcSMSMessage,"#PAYMENT", lcAmount).
   END.
   ELSE IF iiPhase = {&Q25_MONTH_24_CHOSEN} THEN DO:
   /* Q25 Month 24 20th day extension made */  
      ASSIGN
         lcSMSMessage = fGetSMSTxt("Q25FinalFeeMsgChosenExt",
                                   TODAY, 1, OUTPUT ldReqStamp)
         lcSMSMessage = REPLACE(lcSMSMessage,"#PAYMENT", lcAmount).
   END.
   IF iiPhase < {&Q25_MONTH_24_FINAL_MSG} THEN DO:
   /* Month 22-24 */
      ASSIGN
         lcEncryptedMSISDN = encrypt_data(icCli,
                             {&ENCRYPTION_METHOD}, lcPassPhrase)
      /* convert some special characters to url encoding (at least '+' char
         could cause problems at later phases. */
         lcEncryptedMSISDN = fUrlEncode(lcEncryptedMSISDN, "query")
         lcSMSMessage = REPLACE(lcSMSMessage, "#MSISDN", lcEncryptedMSISDN).
   END.
   RETURN lcSMSMessage.
END FUNCTION.

/* Logs writings for testing and DWH */
FUNCTION fQ25LogWriting RETURNS LOGICAL
   (INPUT iclogText AS CHAR,
    INPUT iiLogLevel AS INT,
    INPUT iilogPhase AS INT,
    INPUT iiExecType AS INT).
   DEF VAR lcQ25LogType AS CHAR NO-UNDO. 
   /* Requested customer log writings. YPR-3446 */
   IF iiExecType EQ {&Q25_EXEC_TYPE_CUST_LOG_GENERATION} THEN DO:
      /* Only cust level logs are needed to be written here  */
      IF iiLogLevel EQ {&Q25_LOGGING_CUST_LOGS} THEN DO: 
         OUTPUT STREAM Sout TO VALUE(lcQ25DWHLogFile) APPEND.
         PUT STREAM Sout UNFORMATTED
            icLogText SKIP.
         OUTPUT STREAM Sout CLOSE. 
         RETURN TRUE.
      END.
      ELSE RETURN TRUE. /* no other than cust logs needed at this phase */
   END.
   ELSE IF iiExecType EQ {&Q25_EXEC_TYPE_HRLP_UNIV} THEN DO:
      lcHRLPLogFile = lcHRLPSpoolDir + "IFS_Q25HR_UNIVERSE_" + 
                      (SUBSTRING(STRING(fMakeTS()),1,8)) + ".LOG".
      OUTPUT STREAM Sout TO VALUE(lcHRLPLogFile) APPEND.
      PUT STREAM Sout UNFORMATTED
         icLogText SKIP.
      OUTPUT STREAM Sout CLOSE.

   END.
   ELSE IF iiExecType EQ {&Q25_EXEC_TYPE_HRLP_ACT} THEN DO:
      lcHRLPLogFile = lcHRLPSpoolDir + "IFS_Q25HR_ACTIVE_" + 
                      (SUBSTRING(STRING(fMakeTS()),1,8)) + ".LOG".
      OUTPUT STREAM Sout TO VALUE(lcHRLPLogFile) APPEND.
      PUT STREAM Sout UNFORMATTED
         icLogText SKIP.
      OUTPUT STREAM Sout CLOSE.

   END.
   ELSE IF iiExecType EQ {&Q25_EXEC_TYPE_HRLP_REL} THEN DO:
      lcHRLPLogFile = lcHRLPSpoolDir + "IFS_Q25HR_RELEASE_" + 
                      (SUBSTRING(STRING(fMakeTS()),1,8)) + ".LOG".
      OUTPUT STREAM Sout TO VALUE(lcHRLPLogFile) APPEND.
      PUT STREAM Sout UNFORMATTED
         icLogText SKIP.
      OUTPUT STREAM Sout CLOSE.
   END.
   ELSE DO:
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
END.

/* Function to calculate dynamically pause value between message sending. 
   To ensure all messages will be sent before defined end time. */
FUNCTION fCalculateMaxPauseValue RETURN INTEGER
   (INPUT iiToBeSend AS INT).
   DEF VAR ldEndTime AS DEC NO-UNDO.
   DEF VAR ldTimeLeft AS DEC NO-UNDO.
   ASSIGN
      ldEndTime = fHMS2TS(TODAY, lcSendingEndTime)
      ldTimeLeft = (ldEndTime - fMakeTS()) * 100000.
   IF iiToBeSend = 0 THEN RETURN 0. /* no messages left, no pause needed and
                                       do not divide by zero */
   RETURN INT(TRUNC(ldTimeLeft / iiToBeSend,0)). 
END.

/* Template filename for customer logfiles */
FUNCTION fgetTemplateName RETURN CHARACTER
   (INPUT iiPhase AS INT).
   IF iiPhase EQ {&Q25_MONTH_22} OR iiPhase EQ {&Q25_MONTH_23} THEN
      RETURN "Q25ReminderMonth22and23".
   ELSE IF iiPhase EQ {&Q25_MONTH_24} THEN
      RETURN "Q25ReminderMonth24".
   ELSE IF iiPhase EQ {&Q25_MONTH_24_FINAL_MSG} THEN
      RETURN "Q25FinalFeeMsgNoDecision".
   ELSE 
      RETURN "Error: Q25_Phase".
END.

FUNCTION fisQ25TerminalReturned RETURNS LOGICAL
   (INPUT iiOrderId AS INT):
   FIND FIRST TermReturn WHERE
              TermReturn.OrderId EQ iiOrderId NO-LOCK NO-ERROR.

   IF AVAIL TermReturn AND
      ((TermReturn.DeviceScreen = TRUE AND
        TermReturn.DeviceStart = TRUE) OR
       (TermReturn.DeviceScreen = ? AND
        TermReturn.DeviceStart  = ?)) THEN RETURN TRUE. 
      /* Accepted return of device */
   ELSE
      RETURN FALSE.
END.

FUNCTION fisQ25ExtensionDone RETURNS LOGICAL
   (INPUT iiMsSeq AS INT):

   DEF BUFFER bDCCLI FOR DCCLI.

   FIND FIRST bDCCLI NO-LOCK WHERE
              bDCCLI.Brand   EQ gcBrand AND
              bDCCLI.DCEvent EQ "RVTERM12" AND
              bDCCLI.MsSeq   EQ iiMsseq AND
              bDCCLI.ValidTo >= TODAY NO-ERROR.
   RETURN (AVAIL bDCCLI).

END.

FUNCTION fisQ25PendingRequest RETURNS LOGICAL
   (INPUT iiMsSeq AS INT):

   DEF VAR liLoop AS INT.
   DEF VAR liReqStatus AS INT NO-UNDO. 

   DO liLoop = 1 TO NUM-ENTRIES({&REQ_ONGOING_STATUSES}):

      liReqStatus = INT(ENTRY(liLoop,({&REQ_ONGOING_STATUSES}))).

      IF CAN-FIND(FIRST MsRequest NO-LOCK WHERE
                        MsRequest.msSeq EQ iimsseq AND
                        MsRequest.reqtype EQ 8 AND
                        MsRequest.ReqStatus = liReqStatus AND
                        MsRequest.ReqCParam3 EQ "RVTERM12") THEN
         RETURN TRUE.
   END.
   RETURN FALSE.
END.

FUNCTION fisQ25RenewalDone RETURNS LOGICAL
   (INPUT iiMsSeq AS INT,
    INPUT idaFromDate AS DATE):

   DEF VAR ldaMonth22Date AS DATE NO-UNDO.
   DEF BUFFER Order FOR Order.

   /* calculate first day of the month 22, Renewal have to be after 
      that date to be considered Q25 action */
   ASSIGN
      ldaMonth22Date = ADD-INTERVAL(idaFromDate, 22, 'months':U)
      ldaMonth22Date = DATE(MONTH(ldaMonth22Date),1,YEAR(ldaMonth22Date)).   

   FOR EACH Order NO-LOCK WHERE
            Order.MsSeq = iiMsseq AND
            Order.OrderType = {&ORDER_TYPE_RENEWAL} AND
            Order.CrStamp > fHMS2TS(ldaMonth22Date, "00:00:00"):

       IF CAN-FIND(FIRST MsRequest NO-LOCK WHERE
                         MsRequest.MsSeq = Order.MsSeq AND
                         MsRequest.ReqType = 49 AND
                         MsRequest.ReqIParam1 EQ Order.OrderId) THEN NEXT.

       RETURN TRUE. /* Renewal / Renuvo done */
   END.

   RETURN FALSE.
END.

FUNCTION fisQ25ExtensionAllowed RETURNS LOGICAL
   (BUFFER Singlefee FOR SingleFee,
    OUTPUT ocLogText AS CHAR):

   DEF VAR liMsSeq AS INT NO-UNDO. 
   DEF VAR liPerContrId AS INT NO-UNDO. 
   DEF VAR ldaQ25Period AS DATE NO-UNDO.

   DEF BUFFER Mobsub FOR Mobsub.
   DEF BUFFER DCCLI FOR DCCLI.

   IF SingleFee.SourceTable NE "DCCLI" THEN RETURN FALSE.
   IF SingleFee.HostTable NE "MobSub" THEN RETURN FALSE.
   IF SingleFee.OrderID EQ 0 THEN RETURN FALSE.

   ASSIGN
      liMsSeq = INT(SingleFee.KeyValue)
      liPerContrId = INT(SingleFee.SourceKey) NO-ERROR.

   IF ERROR-STATUS:ERROR THEN RETURN FALSE.
    
   ldaQ25Period = fPer2Date(SingleFee.BillPeriod, 0).

   FIND FIRST DCCLI USE-INDEX PerContractId NO-LOCK WHERE
              DCCLI.PerContractId = liPerContrId AND
              DCCLI.MsSeq = liMsseq AND
              DCCLI.DCEvent BEGINS "PAYTERM" NO-ERROR.

   IF NOT AVAIL DCCLI THEN DO:
      ocLogText = "Q25 NO DCCLI FOUND".
      RETURN FALSE.
   END.
   
   /* No DCCLI for example between start and end date, singlefee is for
      whole month or no DCCLI for some error case (?). */
   IF DCCLI.TermDate NE ? THEN DO:
      ASSIGN
         ocLogText = "Q25 DCCLI Terminated".
      RETURN FALSE.
   END.

   FIND FIRST Mobsub NO-LOCK WHERE
              Mobsub.MsSeq = liMsseq AND
              Mobsub.Paytype = FALSE NO-ERROR.
   IF NOT AVAIL Mobsub THEN DO:
      ocLogText = "Q25 Mobsub not found or prepaid".
      RETURN FALSE.
   END.
            
   IF SingleFee.Billed AND
      NOT CAN-FIND(FIRST Invoice NO-LOCK WHERE
                         Invoice.Invnum = SingleFee.InvNum aND
                         Invoice.InvType = 99) THEN DO:
      ocLogText = "Q25 Residual fee Billed".
      RETURN FALSE.
   END.

   IF fisQ25TerminalReturned(SingleFee.OrderId) THEN DO:
      ocLogText = "Q25 Device returned".
      RETURN FALSE.
   END.

   IF fisQ25ExtensionDone(liMsseq) THEN DO:
      ocLogText = "Q25 already done".
      RETURN FALSE.
   END.

   IF fisQ25RenewalDone(liMsseq, DCCLI.ValidFrom) THEN DO: 
      ocLogText = "Q25 Renewal done".
      RETURN FALSE.
   END.
   
   IF fisQ25PendingRequest(liMsseq) THEN DO:
      ocLogText = "Q25 Pending Request".
      RETURN FALSE.
   END.
   
   IF NOT (DCCLI.ValidTo >= ldaQ25Period - 1 AND
           DCCLI.ValidTo <= fLastDayOfMonth(ldaQ25Period)) THEN DO:
      ocLogText = "Q25 billperiod and installment end date differ".
      RETURN FALSE.
   END.

   RETURN TRUE.
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
   DEF VAR lcSMSMessage      AS CHAR NO-UNDO.
   DEF VAR liSentCount       AS INT NO-UNDO.
   DEF VAR lcLogText         AS CHAR NO-UNDO.
   DEF VAR liPauseValue      AS INT NO-UNDO.
   DEF VAR liCalcPauseValue  AS INT NO-UNDO.
   DEF VAR lcTemplateName    AS CHAR NO-UNDO.
   DEF VAR liLogType         AS INT NO-UNDO.  

   DEF BUFFER DCCLI  FOR DCCLI.
   DEF BUFFER Mobsub FOR Mobsub.

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

   FOR EACH SingleFee NO-LOCK WHERE
            SingleFee.Brand       = gcBrand AND
            SingleFee.Billcode BEGINS "RVTERM" AND
            SingleFee.HostTable   = "Mobsub" AND
            SingleFee.SourceTable = "DCCLI" AND
            SingleFee.CalcObj     = "RVTERM" AND
            SingleFee.BillPeriod  = liPeriod AND
            SingleFee.OrderId > 0:
      
      FIND FIRST Mobsub NO-LOCK WHERE
                 Mobsub.MsSeq = INT(SingleFee.KeyValue) NO-ERROR.
            
      IF NOT fisQ25ExtensionAllowed(BUFFER SingleFee,
                                    OUTPUT lcLogText) THEN DO:

         IF lcLogText > "" THEN
            lcLogText = lcLogText + ": " + 
                        STRING(iiphase) + "|" +
                        (IF AVAIL Mobsub THEN
                           STRING(Mobsub.CLI) ELSE "") + "|" + 
                        STRING(SingleFee.KeyValue) + "|" + 
                        STRING(SingleFee.Amt).
                        
            fQ25LogWriting(lcLogText, {&Q25_LOGGING_DETAILED}, iiphase,
                           iiExecType).
         NEXT.
      END.

      IF NOT AVAIL Mobsub THEN NEXT.
      
      liCount = liCount + 1. /* Full q25 count in Month */
   
      IF(iiExecType EQ {&Q25_EXEC_TYPE_SMS_SENDING}) THEN DO:
         oiTotalCountLeft = oiTotalcountLeft - 1.

         FIND FIRST SMSMessage NO-LOCK WHERE 
                    SMSMessage.msseq = MobSub.MsSeq AND
                    SMSMessage.CreStamp > fDate2TS(TODAY) AND
                    SMSMessage.SMSType = {&SMS_TYPE_Q25} NO-ERROR.
         
         IF AVAIL SMSMessage AND (lcTestStartDay = "" OR 
                                  lcTestStartDay = ?) THEN DO:
            /* Something have went wrong, SMS sending for today already marked
               for this subscriber. If teststartday is defined testing ongoing
               and SMS messaging allowed. */
            ASSIGN
               lcLogText = "SMS Already created: " +
                           STRING(iiphase) + "|" + STRING(Mobsub.CLI) + "|" +
                           STRING(Mobsub.MsSeq) + "|" + STRING(SingleFee.Amt).         
            fQ25LogWriting(lcLogText, {&Q25_LOGGING_DETAILED}, iiphase,
                                       iiExecType).
            NEXT.
         END.
         ELSE DO:
            lcSMSMessage = fgetQ25SMSMessage(iiphase, SingleFee.BillPeriod, 
                                             SingleFee.Amt, MobSub.CLI).
            /* Send SMS */
            fCreateSMS(SingleFee.CustNum,
                       MobSub.Cli,
                       MobSub.MsSeq,
                       SingleFee.OrderId,
                       lcSMSMessage,
                       "622",
                       {&SMS_TYPE_Q25}).
            ASSIGN
               liSentCount = liSentCount + 1
               lcLogText = STRING(iiphase) + "|" + STRING(MobSub.CLI) + "|" +
                        STRING(MobSub.MsSeq).
            fQ25LogWriting(lcLogText, {&Q25_LOGGING_SENT_MSGS}, iiphase,
                           iiExecType).
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
         liLogType = {&Q25_LOGGING_DETAILED}.
         /* Some logging about SMSs to be send. */
         IF iiphase = {&Q25_MONTH_24_CHOSEN} THEN DO:
            /* Q25 Month 24 20th day extension made, write only internal log */
            
            IF (iiExecType NE {&Q25_EXEC_TYPE_CUST_LOG_GENERATION}) THEN DO:
               lcLogText = "Send SMS Q25 Chosen: " +
                        STRING(iiphase) + "|" + STRING(MobSub.CLI) + "|" + 
                        STRING(MobSub.MsSeq).
            END.
         END.
         ELSE DO:
            lcTemplateName = fgetTemplateName(iiphase).  
            IF (iiExecType EQ {&Q25_EXEC_TYPE_CUST_LOG_GENERATION}) AND
               lcTemplateName BEGINS "Q25" THEN DO:
            ASSIGN   
               lcLogText = MobSub.CLI + ";" + 
                           STRING(ldaExecuteDate,"99/99/9999") + ";" +
                           lcTemplateName
               liLogType = {&Q25_LOGGING_CUST_LOGS}.
            END.
            ELSE DO:
               lcLogText = "Send SMS: " +
                           STRING(iiphase) + "|" + STRING(MobSub.CLI) + "|" +
                           STRING(MobSub.MsSeq) + "|" +
                           STRING(SingleFee.Amt) + "|" + lcTemplateName.
            END.
         END.   
         fQ25LogWriting(lcLogText, liLogType, iiphase,
                        iiExecType).
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
      lcLogText = lcLogText + STRING(liCount) + 
         "|" + STRING(etime / 1000).
      fQ25LogWriting(lcLogText, {&Q25_LOGGING_COUNTERS}, iiphase,
                     iiExecType).
      RETURN liCount.
   END.
   RETURN oiTotalCountLeft.
END FUNCTION.


FUNCTION fBankByBillCode RETURNS CHAR
   (icBillCode AS CHAR):
   CASE icBillCode:
      WHEN "RVTERM1EF" THEN RETURN "UNO-E".
      WHEN "RVTERMBSF" THEN RETURN "Yoigo". /* "Sabadell" - YPR-3565 */
      WHEN "RVTERMF" THEN RETURN "Yoigo".
   END CASE.
   RETURN "".
END.

FUNCTION getQ25phase RETURNS INT
   (INPUT iimsseq AS INT,
    INPUT iiCustNum AS INT):

   DEF VAR liPhase AS INT NO-UNDO.
   DEF VAR liPeriod AS INT NO-UNDO.
   DEF VAR lcLogText AS CHAR NO-UNDO.
   DEF VAR ldaDate AS DATE NO-UNDO. 

   /* Loop through Q25 phases starting on nearest to end (Month 24) */
   DO liPhase = {&Q25_MONTH_24} TO {&Q25_MONTH_22}:
      /* set needed period */
      ASSIGN
         ldaDate = ADD-INTERVAL(TODAY, liPhase, 'months':U)
         liPeriod = YEAR(ldaDate) * 100 + MONTH(ldaDate).

      FOR EACH SingleFee NO-LOCK WHERE
               SingleFee.Brand       EQ gcBrand AND
               SingleFee.CustNum     EQ iiCustNum AND
               SingleFee.HostTable   EQ "Mobsub" AND
               SingleFee.Keyvalue    EQ STRING(iimsseq) AND
               SingleFee.BillPeriod  EQ liPeriod AND
               SingleFee.SourceTable EQ "DCCLI" AND
               SingleFee.CalcObj     EQ "RVTERM" AND
               SingleFee.Billcode    BEGINS "RVTERM" AND
               SingleFee.OrderId > 0:

         IF NOT fisQ25ExtensionAllowed(BUFFER SingleFee,
                                       lcLogText) THEN NEXT.

         RETURN liPhase.
      END.
   END.
   RETURN {&Q25_NOT_ACTION_PHASE}. /* Not Q25 phase M22-M24 customer */
END.

/*Function makes HRLP parametr initializations.
This must be called in programs that are handling HRLP related data.*/
FUNCTION fInitHRLPParameters RETURNS CHAR
   ():
   ASSIGN

      lcLandingPageLink = fCParam("HRLP","HRLPLandingpage")
      lcPassPhrase = fCParam("Q25","Q25PassPhrase")
      lcHRLPOutDir = fCParam("HRLP","HRLPOutDir")
      lcHRLPListInDir = fCParam("HRLP","HRLPListInDir")
      lcHrlpRemRedirDir = fCParam("HRLP","HrlpRemRedirDir")
      lcHRLPLogDir = fCParam("HRLP","HRLPLogDir")
      lcHRLPSpoolDir = fCParam("HRLP","HRLPSpoolDir")
      lcHRLPInProcDir = fCParam("HRLP","HRLPInProcDir")
      lcHRLPTestMSSeq = fCParam("HRLP","HRLPTestMSSeq")
      liHRLPTestLevel = fCParamI("HRLPTestLevel").
 
   IF lcHRLPOutDir EQ "" OR lcHRLPOutDir EQ ? THEN lcHRLPOutDir = "/tmp/".
   IF lcHRLPListInDir EQ "" OR lcHRLPListInDir EQ ? THEN lcHRLPlistInDir = "/tmp/".
  IF lcHRLPRemRedirDirDir EQ "" OR lcHRLPRemRedirDirDir EQ ? THEN
    lcHRLPRemRedirDirDir = "/tmp/".
   IF lcHRLPLogDir EQ "" OR lcHRLPLogDir EQ ? THEN lcHRLPLogDir = "/tmp/".


END.   

FUNCTION fGetMonthlyFee RETURNS DECIMAL
   (iiPerContractId AS INT,
    iiMsSeq AS INT):
         FIND FIRST FixedFee WHERE
                    FixedFee.Brand EQ gcBrand AND
                    FixedFee.HostTable EQ "MobSub" AND
                    FixedFee.KeyValue EQ STRING(iiMsseq) AND
                    FixedFee.SourceTable EQ "DCCLI" AND
                    FixedFee.SourceKey EQ STRING(iiPerContractID) AND
                    FixedFee.BillCode BEGINS "PAYTERM"
                    NO-LOCK NO-ERROR.
         IF AVAIL FixedFee THEN RETURN FixedFee.amt.
   RETURN -1.
END.

FUNCTION fWriteIFSData RETURNS LOGICAL
   (INPUT icCli AS CHAR,
    INPUT iiCustNum AS INT,
    INPUT idaQ25Date AS DATE,
    INPUT idMonthlyFee AS DEC,
    INPUT idQ25Amount AS DEC):
   DEF VAR lcHRLPDelim AS CHAR NO-UNDO.
   DEF VAR lcLogText AS CHAR NO-UNDO.
   DEF VAR lcEncryptedMSISDN AS CHAR NO-UNDO.
   DEF VAR lcLPLink AS CHAR NO-UNDO.
   ASSIGN
      lcEncryptedMSISDN = encrypt_data(MobSub.CLI,
                          {&ENCRYPTION_METHOD}, lcPassPhrase)
   /* convert some special characters to url encoding (at least '+' char
      could cause problems at later phases. */
      lcEncryptedMSISDN = fUrlEncode(lcEncryptedMSISDN, "query")
      lcLPLink = REPLACE(lcLandingpageLink, "#MSISDN", lcEncryptedMSISDN)

   /*verifications done, write data to IFS file*/
      lcHRLPDelim = {&Q25_HRLP_DELIM}
      lcLogText = STRING(iiCustNum)      + lcHRLPDelim + /*Custnumber*/
                  STRING(icCli)          + lcHRLPDelim + /*MSISDN*/
                  STRING(idaQ25Date)      + lcHRLPDelim + /*Q25 month*/
                  STRING(idMonthlyFee)   + lcHRLPDelim + /*installment value*/
                  STRING(idQ25Amount)       + lcHRLPDelim + /*Q25 value*/
                  STRING(lcLPLink).                      /*LP Link*/

    OUTPUT STREAM SHRLP TO VALUE(lcHRLPOutFile) APPEND.
    PUT STREAM SHRLP UNFORMATTED lcLogText SKIP.
    OUTPUT STREAM SHRLP CLOSE.
END.

/* SMS message generating and sending for Q25. */
FUNCTION fGenerateQ25List RETURNS INTEGER:

   DEF VAR liPeriod AS INT NO-UNDO.
   DEF VAR ldaEndDate AS DATE NO-UNDO.
   DEF VAR liMsSeq AS INT NO-UNDO.
   DEF VAR liPerContrId AS INT NO-UNDO.
   DEF VAR ldAmount AS DECIMAL NO-UNDO.
   DEF VAR ldMonthlyFee AS DECIMAL NO-UNDO.
   DEF VAR lcHRLPDelim AS CHAR NO-UNDO.
   DEF VAR lcLogText AS CHAR NO-UNDO.
   DEF VAR lcEncryptedMSISDN AS CHAR NO-UNDO.
   DEF VAR lcLPLink AS CHAR NO-UNDO.
   DEF VAR liLoop AS INT NO-UNDO.

   ASSIGN
      ldaEndDate = fLastDayOfMonth(TODAY)
      liPeriod = YEAR(TODAY) * 100 + MONTH(TODAY).

   lcHRLPOutFile = lcHRLPSpoolDir + "IFS_Q25HR_UNIVERSE_" +
                   REPLACE(STRING(fMakeTS()),".","_") +  ".DAT".
   IF liHRLPTestLevel EQ {&Q25_HRLP_FULL_TEST} THEN DO:
   /* Testing with non Q25 subscriber. Need to generate list file
      with hardcoded values so it looks like Q25 case */
      DO liLoop = 1 TO NUM-ENTRIES(lcHRLPTestMSSeq): 
         FIND FIRST MobSub NO-LOCK WHERE
                    Mobsub.MsSeq = INT(ENTRY(liLoop, lcHRLPTestMSSeq)) NO-ERROR.
         IF AVAIL MobSub THEN
            fWriteIFSData(Mobsub.CLI, MobSub.CustNum, DATE(ldaEndDate + 1),
                          12.21, 121.21).
      END.
   END.   
   ELSE DO:
      /* Normal functionality or testlevel = 1 (testing with real Q25
         Subscriptions. If testing is ongoing, only listed subscriptions
         will be included to file. */
      FOR EACH SingleFee USE-INDEX BillCode WHERE
               SingleFee.Brand       = gcBrand AND
               SingleFee.Billcode BEGINS "RVTERM" AND
               SingleFee.HostTable   = "Mobsub" AND
               SingleFee.SourceTable = "DCCLI" AND
               SingleFee.CalcObj     = "RVTERM" AND
               SingleFee.BillPeriod  = liPeriod NO-LOCK:
         IF SingleFee.OrderId <= 0 THEN NEXT.
         ASSIGN
            ldAmount = Singlefee.Amt
            liMsseq = INT(SingleFee.KeyValue)
            liPerContrId = INT(SingleFee.sourcekey).
         /* If test ongoing, SKIP any other subscriptions that listed ones */
         IF (liHRLPTestLevel EQ {&Q25_HRLP_ONLY_PROV_TEST}) AND
            (LOOKUP(STRING(liMsSeq),lcHRLPTestMSSeq) EQ 0) 
            THEN NEXT.
         
         /*Function checks also Extension, TermReturn and Renewal*/
         IF NOT fisQ25ExtensionAllowed(BUFFER SingleFee, 
                                   lcLogText) THEN DO:
            IF lcLogText > "" THEN
               fQ25LogWriting(lcLogText, {&Q25_LOGGING_DETAILED}, {&Q25_MONTH_24},
                               {&Q25_EXEC_TYPE_HRLP_UNIV}).
            NEXT.
         END.
         
         fWriteIFSData(Mobsub.CLI, MobSub.CustNum, DATE(ldaEndDate + 1),
                       fGetMonthlyFee(liPerContrId,liMsSeq), ldAmount).
      END.
   END.
   fMove2TransDir(lcHRLPLogFile, "", lcHRLPLogDir).
   fMove2TransDir(lcHRLPOutFile, "", lcHRLPOutDir).
END FUNCTION.

FUNCTION fMakeProdigyRequest RETURNS LOGICAL
   (INPUT iiMsSeq   AS INT,
    INPUT iiCustNum AS INT,
    INPUT icCommand AS CHAR,
    INPUT-OUTPUT ocLine AS CHAR):
   DEF VAR liReq AS INT NO-UNDO.
   DEF VAR lcError AS CHAR NO-UNDO.
   DEF VAR lcMemoTitle AS CHAR NO-UNDO.
   DEF VAR lcMemoText AS CHAR NO-UNDO.
   DEF VAR lcResult AS CHAR NO-UNDO.   
   /* Create subrequests (set mandataory and orig request) */
   liReq = fServiceRequest (iiMsSeq,
                            "LP",
                            1,
                            icCommand,
                            fSecOffSet(fMakeTS(),5),
                            "",                /* SalesMan */
                            FALSE,             /* Set fees */
                            FALSE,             /* SMS */
                            "",
                            "",
                            0,
                            FALSE,
                            OUTPUT lcError).

   /* Creation of subrequests failed, "fail" master request too */
   IF liReq = 0 OR liReq = ? THEN DO:
      fReqStatus(3,"ServiceRequest failure: " + lcError).
      ocLine = ocLine + {&Q25_HRLP_DELIM} + "Error: ServiceRequest failure".
      RETURN FALSE.
   END.
   ELSE DO:
      ocLine = ocLine + {&Q25_HRLP_DELIM} + icCommand + " Sent successfully".
      lcMemoTitle = "LP Riesgo Pago Final".
      IF icCommand BEGINS "REDIRECTION" THEN
         lcMemotext = "Redirección a LP Pago Final Riesgo activada".
      ELSE IF icCommand EQ "remove" THEN
         lcMemotext = "IFS elimina la LP Riesgo Pago Final sin que el " +
                      "cliente la haya visto".
      ELSE
         lcMemotext = "Unknown redirection command". /* should not ever 
                                                        come here */
      DYNAMIC-FUNCTION("fWriteMemoWithType" IN ghFunc1,
                 "Mobsub",
                 STRING(iiMsSeq),
                 iiCustNum,
                 lcMemoTitle,
                 lcMemoText,
                 "Service",  /* memo type */
                 "IFS").     /* creator */
   END.
   RETURN TRUE.
END.    

