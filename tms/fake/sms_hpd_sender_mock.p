/* ----------------------------------------------------------------------
  MODULE .......: sms_hpd_sender_mock.p
  TASK .........: Sends SMS messages to HPD SMS sender
  APPLICATION ..: TMS
  AUTHOR .......: anttis
  CREATED ......: 29.03.15
  Version ......: Yoigo
----------------------------------------------------------------------- */
{commpaa.i}
katun = "Cron".
gcBrand = "1".
{timestamp.i}
{log.i}
{ftransdir.i}
{replog_reader.i}
{smsmessage.i}

FORM
   SKIP
   "ROUND:" liLoop FORMAT ">>>>>>9"
   ldToday  FORMAT "99.99.9999"
   lcTime
   "Sent SMS Qty:" liRepLogs FORMAT ">>>>>>>9" 
WITH
   ROW 6 WIDTH 60 CENTERED OVERLAY NO-LABEL
   TITLE " SMS HPD Sender Mock"
FRAME frmLog.

/* Initialize the configurations */
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

   PUT SCREEN ROW 22 COL 1
   "F8 TO QUIT, OTHER KEYS START REPLOG IMMEDIATELLY".
   
   READKEY PAUSE 10.

   IF LOOKUP(KEYLABEL(LASTKEY),"F8,8") > 0 THEN LEAVE RepLog.

END.

RUN pFinalize(INPUT "sms").

QUIT.

PROCEDURE pHandleSMSQueue:

   DEFINE OUTPUT PARAMETER oiHandled AS INTEGER   NO-UNDO.

   DEF VAR ldeTS AS DEC NO-UNDO. 
   DEF VAR lcMessage AS CHAR NO-UNDO. 
   
   ldeTS = fMakeTS().

   LOOP:
   FOR EACH SMSMessage EXCLUSIVE-LOCK WHERE
            SMSMessage.DeliType  = {&SMS_DELITYPE_OUT}   AND
            SMSMessage.DeliStat  = {&SMS_DELISTATUS_NEW} AND
            SMSMessage.ActStamp <= ldeTS:
      
      /* Treshold value */
      IF oiHandled >= 10000 THEN LEAVE LOOP.
   
      lcMessage =
         "SMS" + lcDel +
         fNotNull(SMSMessage.MSISDN) + lcDel +
         fNotNull(STRING(SMSMessage.SMSSeq)) + lcDel +
         fNotNull(STRING(fTimeStamp2DateTime(ldeTS))) + lcDel +
         fNotNull(SMSMessage.DeliMsg) + lcDel + 
         fNotNull(STRING(SMSMessage.MsSeq)) + lcDel +
         fNotNull(SMSMessage.OrigAddress) + lcDel + 
         fNotNull(STRING(SMSMessage.OrderId)).

         
      ASSIGN
         SMSMessage.DeliStamp = fMakeTS()
         SMSMessage.DeliStat = {&CA_DELISTAT_SENT}.

      oiHandled = oiHandled + 1.
         
   END.

END PROCEDURE.
