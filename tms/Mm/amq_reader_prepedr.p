/* ----------------------------------------------------------------------
  MODULE .......: Mm/amq_reader_prepedr.p
  TASK .........: ActiveMQ PrepEDR reader for high performance data
  APPLICATION ..: tms
  AUTHOR .......: Ivailo
  CREATED ......: 20.02.15
  Version ......: yoigo
---------------------------------------------------------------------- */

{Syst/commpaa.i}
gcBrand = "1".
Katun = "Cron".
{Func/timestamp.i}
{Func/cparam2.i}
{Func/direct_dbconnect.i}
{Func/replog_reader.i}

FORM
   SKIP
   "ROUND:" liLoop   FORMAT ">>>>>>9"
   ldToday  FORMAT "99.99.9999"
   lcTime
   "PrepEDR Qty:" liCdrCount FORMAT ">>>>>>>9"
WITH
   ROW 6 WIDTH 60 CENTERED OVERLAY NO-LABEL
   TITLE " ActiveMQ PrepEDR Reader "
FRAME frmLog.

IF lcNagiosURL > "" THEN
   fKeepAlive("REPLOG_PREPEDR:PrepEDR Database Reader",lcNagiosURL).
PUT SCREEN ROW 1 COL 1 "NAGIOS: " + STRING(today,"99-99-9999") + " " +
                                    STRING(time,"hh:mm:ss").

RUN pInitialize(INPUT "amq_prepedr").

IF RETURN-VALUE > "" THEN DO:
   IF LOG-MANAGER:LOGGING-LEVEL GE 1 THEN
      LOG-MANAGER:WRITE-MESSAGE(RETURN-VALUE, "ERROR").
   RETURN RETURN-VALUE.
END.

/* Call ActiveMQ Publisher class */
lMsgPublisher = NEW Gwy.MqPublisher(lcHost,liPort,
                                    liTimeOut,"hpd.prepedr",
                                    lcUserName,lcPassword).

IF NOT VALID-OBJECT(lMsgPublisher) THEN DO:
   IF LOG-MANAGER:LOGGING-LEVEL GE 1 THEN
      LOG-MANAGER:WRITE-MESSAGE("ActiveMQ Publisher handle not found","ERROR").
END.

PrepEDR:
DO WHILE TRUE :

   RUN pAmqEDRReader(OUTPUT liAmount).

   /* reset the counter at midnight */
   IF ldaCounterDate = TODAY THEN liCdrCount = liCdrCount + liAmount.
   ELSE ASSIGN liCdrCount     = 0
               ldaCounterDate = TODAY.

   liLoop = liLoop + 1.
   PAUSE 0.
   DISP  liLoop  
         Today @ ldToday 
         liCdrCount
         string(time,"hh:mm:ss") @ lctime
   WITH FRAME frmLog.
   PAUSE 0.
  
   PUT SCREEN ROW 22 COL 1
   "F8 TO QUIT, OTHER KEYS START PrepEDR READER IMMEDIATELLY".

   READKEY PAUSE liCdrFreq. /* the default is 120 (2 min.) */

   IF lcNagiosURL > "" THEN
      fKeepAlive("REPLOG_PREPEDR:PrepEDR Database Reader",lcNagiosURL).
   PUT SCREEN ROW 1 COL 1 "NAGIOS: " + STRING(today,"99-99-9999") + " " +
                                       STRING(time,"hh:mm:ss").
   IF KEYLABEL(LASTKEY) = "F8" THEN DO:
      ASSIGN llOk = FALSE.
      MESSAGE "ARE YOU SURE YOU WANT TO QUIT (Y/N) ? " UPDATE llOk.
      IF llOk THEN LEAVE PrepEDR.
   END.

END.

RUN pFinalize(INPUT "amq_prepedr").

QUIT.

PROCEDURE pAmqEDRReader:

   DEFINE OUTPUT PARAMETER oiHandled AS INTEGER   NO-UNDO.

   ASSIGN ldaReadDate  = TODAY
          ldeReadInTS  = fMake2Dt(ldaReadDate,TIME)
          ldeCurrStamp = fMakeTS().

   DO TRANS:
      FIND FIRST ActionLog WHERE
                 ActionLog.Brand     = gcBrand        AND
                 ActionLog.ActionID  = "PrepEDR_HPD"   AND
                 ActionLog.TableName = "PrepEDR" EXCLUSIVE-LOCK NO-ERROR.
      IF AVAIL ActionLog THEN DO:
         ldeReadInTS = ActionLog.ActionDec.
         fSplitTS(ldeReadInTS,ldaReadDate,liReadTime).
         IF llStart THEN ASSIGN ActionLog.ActionTS = fMakeTS()
                                llStart = FALSE.
      END.
      ELSE DO:
         CREATE ActionLog.
         ASSIGN 
            ActionLog.Brand        = gcBrand
            ActionLog.TableName    = "PrepEDR"
            ActionLog.KeyValue     = "HPD"
            ActionLog.ActionID     = "PrepEDR_HPD"
            ActionLog.ActionPeriod = YEAR(ldaReadDate) * 100 + MONTH(ldaReadDate)
            ActionLog.ActionStatus = 2
            ActionLog.UserCode     = katun
            ActionLog.ActionDec    = ldeReadInTS.
      END. /* ELSE DO: */

      RELEASE ActionLog.
   END. /* DO TRANS: */

   ldeCDRStamp = ldeReadInTS.

   RUN pStartReader(ldaReadDate,
                    liReadTime,
                    fTimeStamp2DateTime(ldeCurrStamp),
                    OUTPUT oiHandled,
                    INPUT-OUTPUT ldeCDRStamp).

   DO TRANS:
      FIND FIRST ActionLog WHERE
                 ActionLog.Brand     = gcBrand AND
                 ActionLog.ActionID  = "PrepEDR_HPD" AND
                 ActionLog.TableName = "PrepEDR" EXCLUSIVE-LOCK NO-ERROR.
      IF AVAIL ActionLog THEN
         ActionLog.ActionDec = ldeCDRStamp.
      RELEASE ActionLog.
   END. /* DO TRANS: */

END PROCEDURE.

PROCEDURE pStartReader:

   DEF INPUT  PARAMETER idaReadDate        AS DATE     NO-UNDO.
   DEF INPUT  PARAMETER iiReadTime         AS INT      NO-UNDO.
   DEF INPUT  PARAMETER idtCurrStamp       AS DATETIME NO-UNDO.
   DEF OUTPUT PARAMETER oiCount            AS INT      NO-UNDO.
   DEF INPUT-OUTPUT PARAMETER odeCDRStamp  AS DEC      NO-UNDO.

   ASSIGN lcDel2     = CHR(255)
          lcKeyValue = ""
          oiCount    = 0.

   DEF VAR liLoopReader AS INTEGER NO-UNDO. 
   DEF VAR ldeBufferTS  AS DECIMAL NO-UNDO. 

   ldeBufferTS = fsecOffset(fMakeTs(),-20).

   Reader:
   DO WHILE idaReadDate <= TODAY:

      liLoopReader = liLoopReader + 1.
      IF liLoopReader > 1 THEN iiReadTime = -1.

      FOR EACH PrepEDR NO-LOCK USE-INDEX ReadDate WHERE
               PrepEDR.ReadDate = idaReadDate AND
               PrepEDR.ReadTime > iiReadTime
               BY ReadTime:

         IF PrepEDR.ErrorCode <> 0 THEN NEXT.

         IF PrepEDR.ReadInTS > ldeBufferTS THEN NEXT.

         ASSIGN lcMessage  = ""
                lcKeyValue = PrepEDR.CLI + lcDel2 + STRING(PrepEDR.DtlSeq) + lcDel2 +
                                                    STRING(PrepEDR.DateSt).

         lcMessage = 
            fNotNull("PrepEDR")                                  + lcDel +
            fNotNull("CREATE")                                   + lcDel +
            fNotNull(STRING(RECID(PrepEDR)))                     + lcDel +
            fNotNull(lcKeyValue)                                 + lcDel +
            fNotNull(STRING(idtCurrStamp))                       + lcDel +
            fNotNull(STRING(PrepEDR.MsSeq))                      + lcDel +
            fNotNull(STRING(PrepEDR.CLI))                        + lcDel +
            fNotNull(STRING(PrepEDR.CustNum))                    + lcDel +
            fNotNull(STRING(PrepEDR.DateSt,"99.99.9999"))        + lcDel +
            fNotNull(STRING(PrepEDR.TimeStart,"HH:MM:SS"))       + lcDel +
            fNotNull(STRING(PrepEDR.ReadInTS))                   + lcDel +
            fNotNull(STRING(PrepEDR.SubscriberFee))              + lcDel +
            fNotNull(STRING(PrepEDR.SuccessCode))                + lcDel +
            fNotNull(STRING(PrepEDR.BalanceAfter)).

         IF lcMessage = ? THEN lcMessage = "".

         IF lMsgPublisher:send_message(lcMessage) THEN
            oiCount = oiCount + 1.
         ELSE DO:
            IF LOG-MANAGER:LOGGING-LEVEL GE 1 THEN
               LOG-MANAGER:WRITE-MESSAGE("Message sending failed","ERROR").
            odeCDRStamp = fsecOffset(odeCDRStamp,-1).
            LEAVE Reader.
         END.

         odeCDRStamp = MAX(odeCDRStamp,PrepEDR.ReadinTS).

         /* Treshold value */
         IF oiCount >= 5000 THEN LEAVE Reader.

         CATCH anyError AS Progress.Lang.Error:
            LOG-MANAGER:WRITE-MESSAGE("Message failed was recovered: " + lcMessage,"DEBUG").
         END CATCH.

      END.

      idaReadDate = idaReadDate + 1.
   END.

END PROCEDURE.
