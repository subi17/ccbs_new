/* ----------------------------------------------------------------------
  MODULE .......: sms_hpd_sender.p
  TASK .........: Sends SMS messages to HPD SMS sender
  APPLICATION ..: TMS
  AUTHOR .......: anttis
  CREATED ......: 29.03.15
  Version ......: Yoigo
----------------------------------------------------------------------- */
{Syst/commpaa.i}
katun = "Cron".
gcBrand = "1".
{Func/timestamp.i}
{Func/log.i}
{Func/ftransdir.i}
{Func/replog_reader.i}
{Func/smsmessage.i}

FORM
   SKIP
   "ROUND:" liLoop FORMAT ">>>>>>9"
   ldToday  FORMAT "99.99.9999"
   lcTime
   "Sent SMS Qty:" liRepLogs FORMAT ">>>>>>>9" 
WITH
   ROW 6 WIDTH 60 CENTERED OVERLAY NO-LABEL
   TITLE " SMS HPD Sender"
FRAME frmLog.

/* Initialize the configurations */
RUN pInitialize(INPUT "sms").
fSetGlobalLoggingLevel(4).

IF RETURN-VALUE > "" THEN DO:
   IF LOG-MANAGER:LOGGING-LEVEL GE 1 THEN
      LOG-MANAGER:WRITE-MESSAGE(RETURN-VALUE, "ERROR").
   RETURN RETURN-VALUE.
END.
/* MAIN BLOCK */

RepLog:
DO WHILE TRUE :

   liLoop = liLoop + 1.
   PAUSE 0.
   DISP  liLoop  
         TODAY @ ldToday 
         liRepLogs
         STRING(TIME,"HH:MM:SS") @ lcTime
   WITH FRAME frmLog.
   PAUSE 0.

   RUN pHandleSMSQueue(OUTPUT liAmount).

   liRepLogs = liRepLogs + liAmount.

   /* Monitoring */
   IF lcNagiosURL > "" THEN
      fKeepAlive("REPLOG_SMS:SMS Sender",lcNagiosURL).

   IF ldLogDate = ? THEN ldLogDate = TODAY.

   /* Replog handling statistics file */
   IF lcLogFileStat > "" AND ldLogDate <> TODAY THEN DO:

      lcStatLogFile = lcLogFileStat + "_sms__" +
                      STRING(YEAR(ldLogDate)) + STRING(MONTH(ldLogDate),"99") +
                      STRING(DAY(ldLogDate),"99") + ".txt".

      OUTPUT STREAM sLogStat TO VALUE(lcStatLogFile).

      FOR EACH ttStat WHERE
               ttStat.ttDatabase  = "OrderCanal" AND
               ttStat.ttDate      = ldLogDate EXCLUSIVE-LOCK:

         PUT STREAM sLogStat UNFORMATTED
             STRING(ttStat.ttDate) "|"
             STRING(ttStat.ttHour) "|"
             ttStat.ttDatabase     "|"
             ttStat.ttTable        "|"
             ttStat.ttEventType    "|"
             ttStat.ttEvents       SKIP.

         DELETE ttStat.
      END.

      OUTPUT STREAM sLogStat CLOSE.

      ldLogDate = TODAY.
   END. /* IF lcLogFileStat > "" AND ldLogDate <> TODAY THEN DO: */
  
   PUT SCREEN ROW 22 COL 1
   "F8 TO QUIT, OTHER KEYS START REPLOG IMMEDIATELLY".
   
   READKEY PAUSE 10.

   IF LOOKUP(KEYLABEL(LASTKEY),"F8,8") > 0 THEN LEAVE RepLog.

END.

RUN pFinalize(INPUT "sms").

QUIT.

PROCEDURE pHandleSMSQueue:

   DEFINE OUTPUT PARAMETER oiHandled AS INTEGER   NO-UNDO.

   DEF VAR liHour AS INTEGER   NO-UNDO.
   DEF VAR ldeTS AS DEC NO-UNDO. 
   DEF VAR lcMessage AS CHAR NO-UNDO. 
   DEFINE VARIABLE llHandled AS LOGICAL NO-UNDO. 
   
   ldeTS = fMakeTS().

   /* Start Replog events dump file */
   IF lcDumpSpool > "" AND
      (ldDate <> TODAY OR ldeTimeStamp <= fMakeTS()) THEN
      RUN pDumpFileRotation(INPUT "sms").
   
   LOOP:
   FOR EACH SMSMessage EXCLUSIVE-LOCK WHERE
            SMSMessage.DeliType  = {&SMS_DELITYPE_OUT}   AND
            SMSMessage.DeliStat  = {&SMS_DELISTATUS_NEW} AND
            SMSMessage.ActStamp <= ldeTS:

      IF VALID-OBJECT(lMsgPublisher) THEN DELETE OBJECT(lMsgPublisher).
      /* Call ActiveMQ Publisher class */
      lMsgPublisher = NEW Gwy.MqPublisher(lcHost,liPort,
                                          liTimeOut,"hpd.sms",
                                          lcUserName,lcPassword).
      IF NOT VALID-OBJECT(lMsgPublisher) THEN DO:
         IF LOG-MANAGER:LOGGING-LEVEL GE 1 THEN
            LOG-MANAGER:WRITE-MESSAGE("ActiveMQ Publisher handle not found","ERROR").
         LEAVE.
      END.
      
      /* Treshold value */
      IF oiHandled >= 10000 THEN LEAVE LOOP.
   
      IF llLogFileAct THEN DO:
         OUTPUT STREAM sDump to VALUE(lcDumpFile).
         llLogFileAct = FALSE.
      END. /* IF llLogFileAct THEN DO: */

      lcMessage =
         "SMS" + lcDel +
         fNotNull(SMSMessage.MSISDN) + lcDel +
         fNotNull(STRING(SMSMessage.SMSSeq)) + lcDel +
         fNotNull(STRING(fTimeStamp2DateTime(ldeTS))) + lcDel +
         fNotNull(SMSMessage.DeliMsg) + lcDel + 
         fNotNull(STRING(SMSMessage.MsSeq)) + lcDel +
         fNotNull(SMSMessage.OrigAddress) + lcDel + 
         fNotNull(STRING(SMSMessage.OrderId)).

      RUN pSendSMS(lcMessage, OUTPUT llHandled).
      
      IF llHandled THEN DO:

         ASSIGN
            SMSMessage.DeliStamp = fMakeTS()
            SMSMessage.DeliStat = {&CA_DELISTAT_SENT}.
   
         oiHandled = oiHandled + 1.
         
         liHour = Time / 3600.

         FIND FIRST ttStat WHERE
                    ttStat.ttDatabase  = "OrderCanal"          AND
                    ttStat.ttTable     = "SMSMessage" AND
                    ttStat.ttEventType = "CREATE" AND
                    ttStat.ttDate      = TODAY             AND
                    ttStat.ttHour      = liHour EXCLUSIVE-LOCK NO-ERROR.
         IF AVAIL ttStat THEN
            ttStat.ttEvents = ttStat.ttEvents + 1.
         ELSE DO:
            CREATE ttStat.
            ASSIGN ttStat.ttDatabase  = "OrderCanal"
                   ttStat.ttTable     = "SMSMessage" 
                   ttStat.ttEventType = "CREATE"
                   ttStat.ttDate      = TODAY
                   ttStat.ttHour      = liHour
                   ttStat.ttEvents    = ttStat.ttEvents + 1.
         END. /* ELSE DO: */
      END.
   END.
   
   IF VALID-OBJECT(lMsgPublisher) THEN DELETE OBJECT(lMsgPublisher).
      
END PROCEDURE.


PROCEDURE pSendSMS:

   DEFINE INPUT PARAMETER icMessage AS CHARACTER NO-UNDO. 
   DEFINE OUTPUT PARAMETER olHandled AS LOGICAL NO-UNDO. 
      
   IF lMsgPublisher:send_message(icMessage) THEN DO:
      olHandled = TRUE.
      fWriteMessage(icMessage).
   END.
   ELSE DO:
      IF LOG-MANAGER:LOGGING-LEVEL GE 1 THEN
         LOG-MANAGER:WRITE-MESSAGE("Message sending failed","ERROR").
      olHandled = FALSE.
   END.

   CATCH anyError AS Progress.Lang.Error:
      LOG-MANAGER:WRITE-MESSAGE("Message failed was recovered: " + 
                                icMessage,"DEBUG").
     olHandled = FALSE.
   END CATCH.

END PROCEDURE. 
