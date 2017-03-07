/* ----------------------------------------------------------------------
  MODULE .......: Mm/amq_reader_rerate_mobcdr.p
  TASK .........: ActiveMQ Rerate MobCDR reader for high performance data
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
   "MobCDR Qty:" liCdrCount FORMAT ">>>>>>>9"
WITH
   ROW 6 WIDTH 60 CENTERED OVERLAY NO-LABEL
   TITLE " ActiveMQ Rerate MobCDR Reader "
FRAME frmLog.

IF lcNagiosURL > "" THEN
   fKeepAlive("REPLOG_MOBCDR:Mcdr Database Reader",lcNagiosURL).
PUT SCREEN ROW 1 COL 1 "NAGIOS: " + STRING(today,"99-99-9999") + " " +
                                    STRING(time,"hh:mm:ss").

RUN pInitialize(INPUT "amq_rerate_mcdr").

IF RETURN-VALUE > "" THEN DO:
   IF LOG-MANAGER:LOGGING-LEVEL GE 1 THEN
      LOG-MANAGER:WRITE-MESSAGE(RETURN-VALUE, "ERROR").
   RETURN RETURN-VALUE.
END.

/* Call ActiveMQ Publisher class */
lMsgPublisher = NEW Gwy.MqPublisher(lcHost,liPort,
                                    liTimeOut,"hpd.mcdr",
                                    lcUserName,lcPassword).

IF NOT VALID-OBJECT(lMsgPublisher) THEN DO:
   IF LOG-MANAGER:LOGGING-LEVEL GE 1 THEN
      LOG-MANAGER:WRITE-MESSAGE("ActiveMQ Publisher handle not found","ERROR").
END.

MobCDR:
DO WHILE TRUE :

   RUN pAmqCDRReader(OUTPUT liAmount).

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
   "F8 TO QUIT, OTHER KEYS START RERATE MobCDR READER IMMEDIATELLY".

   READKEY PAUSE liCdrFreq. /* the default is 120 (2 min.) */

   IF lcNagiosURL > "" THEN
      fKeepAlive("REPLOG_MOBCDR:Mcdr Database Reader",lcNagiosURL).
   PUT SCREEN ROW 1 COL 1 "NAGIOS: " + STRING(today,"99-99-9999") + " " +
                                       STRING(time,"hh:mm:ss").
   IF KEYLABEL(LASTKEY) = "F8" THEN DO:
      ASSIGN llOk = FALSE.
      MESSAGE "ARE YOU SURE YOU WANT TO QUIT (Y/N) ? " UPDATE llOk.
      IF llOk THEN LEAVE MobCDR.
   END.

END.

RUN pFinalize(INPUT "amq_rerate_mcdr").

QUIT.

PROCEDURE pAmqCDRReader:

   DEFINE OUTPUT PARAMETER oiHandled AS INTEGER   NO-UNDO.

   ASSIGN ldaReadDate  = TODAY
          ldeReadInTS  = fMake2Dt(ldaReadDate,TIME)
          ldeCurrStamp = fMakeTS().

   DO TRANS:
      FIND FIRST ActionLog WHERE
                 ActionLog.Brand     = gcBrand        AND
                 ActionLog.ActionID  = "Rerate_MobCDR_HPD"   AND
                 ActionLog.TableName = "MobCDR" EXCLUSIVE-LOCK NO-ERROR.
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
            ActionLog.TableName    = "MobCDR"
            ActionLog.KeyValue     = "HPD"
            ActionLog.ActionID     = "Rerate_MobCDR_HPD"
            ActionLog.ActionPeriod = YEAR(ldaReadDate) * 100 + MONTH(ldaReadDate)
            ActionLog.ActionStatus = 2
            ActionLog.UserCode     = katun
            ActionLog.ActionDec    = ldeReadInTS.
      END. /* ELSE DO: */

      RELEASE ActionLog.
   END. /* DO TRANS: */

   ASSIGN
      ldeCDRStamp    = ldeReadInTS
      ldaConnectDate = ldaReadDate.

   IF NOT CONNECTED("mcdr") THEN
      RUN pDBConnect(ldaConnectDate).
   IF RETURN-VALUE BEGINS "ERROR" THEN RETURN.

   RUN pStartReader(ldaReadDate,
                    liReadTime,
                    fTimeStamp2DateTime(ldeCurrStamp),
                    FALSE,
                    OUTPUT oiHandled,
                    INPUT-OUTPUT ldeCDRStamp).

   /* do two runs if db has been renewed on the event day -> some tickets have
      been saved to old db on the 1. day */
   ldaConnectDate = ?.
   FOR FIRST ttDB WHERE
             ttDb.ConnName = "" AND
             ttDb.TableName = "MobCDR",
       FIRST DBConfig NO-LOCK WHERE
             DBConfig.DBConfigId = ttDb.DbConfigId:
     IF DBConfig.FromDate = ldaReadDate THEN
        ldaConnectDate = DbConfig.FromDate - 1.
   END.

   IF ldaConnectDate NE ? THEN DO:
      RUN pDBConnect(ldaConnectDate).
      IF NOT RETURN-VALUE BEGINS "ERROR" THEN
         RUN pStartReader(ldaReadDate,
                          liReadTime,
                          fTimeStamp2DateTime(ldeCurrStamp),
                          TRUE,
                          OUTPUT oiHandled,
                          INPUT-OUTPUT ldeCDRStamp).
   END.

   DO TRANS:
      FIND FIRST ActionLog WHERE
                 ActionLog.Brand     = gcBrand AND
                 ActionLog.ActionID  = "Rerate_MobCDR_HPD" AND
                 ActionLog.TableName = "MobCDR" EXCLUSIVE-LOCK NO-ERROR.
      IF AVAIL ActionLog THEN
         ActionLog.ActionDec = ldeCDRStamp.
      RELEASE ActionLog.
   END. /* DO TRANS: */

   RELEASE ActionLog.

END PROCEDURE.

PROCEDURE pDBConnect:
 
   DEF INPUT PARAMETER idaConnectDate AS DATE NO-UNDO.
   
   /* connect to correct cdr dbs */
   fInitializeConnectTables("MobCDR,McdrDtl2","").

   RUN pDirectConnect2Dbs(gcBrand,
                          "",
                          idaConnectDate,
                          idaConnectDate).

   IF RETURN-VALUE BEGINS "ERROR" THEN 
      RETURN RETURN-VALUE.

END PROCEDURE.

PROCEDURE pStartReader:

   DEF INPUT  PARAMETER idaReadDate        AS DATE     NO-UNDO.
   DEF INPUT  PARAMETER iiReadTime         AS INT      NO-UNDO.
   DEF INPUT  PARAMETER idtCurrStamp       AS DATETIME NO-UNDO.
   DEF INPUT  PARAMETER llOldDB            AS LOG      NO-UNDO.
   DEF OUTPUT PARAMETER oiCount            AS INT      NO-UNDO.
   DEF INPUT-OUTPUT PARAMETER odeCDRStamp  AS DEC      NO-UNDO.
   
   DEF VAR liLoopReader AS INTEGER NO-UNDO. 
   DEF VAR ldaBuffer    AS DATE NO-UNDO. 
   DEF VAR liBuffer     AS INT NO-UNDO. 

   ASSIGN lcDel2     = CHR(255)
          lcKeyValue = ""
          oiCount    = 0
          liBuffer = TIME
          ldaBuffer = TODAY.

   Reader:
   DO WHILE idaReadDate <= TODAY:

      liLoopReader = liLoopReader + 1.
      IF liLoopReader > 1 THEN iiReadTime = -1.

      FOR EACH EDRHistory NO-LOCK USE-INDEX UpdateDate WHERE
               EDRHistory.Brand       = gcBrand AND
               EDRHistory.UpdateDate  = idaReadDate AND
               EDRHistory.UpdateTime  > iiReadTime,
         FIRST MobCDR NO-LOCK WHERE
               MobCDR.CLI       = EDRHistory.CLI       AND
               MobCDR.DateSt    = EDRHistory.DateSt    AND
               MobCDR.TimeStart = EDRHistory.TimeStart AND
               MobCDR.DtlSeq    = EDRHistory.DtlSeq
               BY EDRHistory.UpdateDate
               BY EDRHistory.UpdateTime:

         IF EDRHistory.UpdateDate > ldaBuffer OR
            (EDRHistory.UpdateDate EQ ldaBuffer AND
             EDRHistory.UpdateTime > liBuffer) THEN NEXT.

         ASSIGN lcMessage  = ""
                lcKeyValue = MobCDR.CLI + lcDel2 + STRING(MobCDR.DtlSeq) + lcDel2 +
                                                   STRING(MobCDR.DateSt).

         lcMessage =
            fNotNull("MobCDR")                                            + lcDel +
            fNotNull(IF MobCDR.ErrorCode = 0 THEN "MODIFY" ELSE "DELETE") + lcDel +
            fNotNull(STRING(RECID(MobCDR)))                               + lcDel +
            fNotNull(lcKeyValue)                                          + lcDel +
            fNotNull(STRING(idtCurrStamp))                                + lcDel +
            fNotNull(STRING(MobCDR.MsSeq))                                + lcDel +
            fNotNull(STRING(MobCDR.CLI))                                  + lcDel +
            fNotNull(MobCDR.CLIType)                                      + lcDel +
            fNotNull(STRING(MobCDR.InvCust))                              + lcDel +
            fNotNull(STRING(MobCdr.DateSt,"99.99.9999"))                  + lcDel +
            fNotNull(STRING(MobCdr.TimeSt,"HH:MM:SS"))                    + lcDel +
            fNotNull(STRING(MobCDR.ReadInTS))                             + lcDel +
            fNotNull(MobCDR.EventType)                                    + lcDel +
            fNotNull(MobCDR.GsmBnr)                                       + lcDel +
            fNotNull(MobCDR.BillCode)                                     + lcDel +
            fNotNull(STRING(MobCDR.CCN))                                  + lcDel +
            fNotNull(STRING(MobCDR.BillDur))                              + lcDel +
            fNotNull(STRING(MobCDR.DataIn + MobCDR.DataOut))              + lcDel +
            fNotNull(TRIM(STRING(MobCDR.Amount,"->>>>>>>>>>>9.9<<<<<")))  + lcDel +
            fNotNull(MobCDR.DCEvent)                                      + lcDel +
            fNotNull(MobCDR.BDest).

         IF lcMessage = ? THEN lcMessage = "".

         IF lMsgPublisher:send_message(lcMessage) THEN
            oiCount = oiCount + 1.
         ELSE DO:
            IF LOG-MANAGER:LOGGING-LEVEL GE 1 THEN
               LOG-MANAGER:WRITE-MESSAGE("Message sending failed","ERROR").
            odeCDRStamp = fsecOffset(odeCDRStamp,-1).
            LEAVE Reader.
         END.

         IF NOT llOldDB THEN
            odeCDRStamp = fMake2Dt(EDRHistory.UpdateDate,EDRHistory.UpdateTime).

         /* Treshold value */
         IF oiCount >= 5000 THEN LEAVE Reader.

         CATCH anyError AS Progress.Lang.Error:
            LOG-MANAGER:WRITE-MESSAGE("Message failed was recovered: " + lcMessage,"DEBUG").
         END CATCH.

      END.

      idaReadDate = idaReadDate + 1.
   END.

END PROCEDURE.
