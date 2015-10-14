/* ----------------------------------------------------------------------
  MODULE .......: Mm/amq_reader_prepcdr.p
  TASK .........: ActiveMQ PrepCDR reader for high performance data
  APPLICATION ..: tms
  AUTHOR .......: Ivailo
  CREATED ......: 20.02.15
  Version ......: yoigo
---------------------------------------------------------------------- */

{commpaa.i}
gcBrand = "1".
Katun = "Cron".
{timestamp.i}
{cparam2.i}
{direct_dbconnect.i}
{replog_reader.i}
{tmsconst.i}

FORM
   SKIP
   "ROUND:" liLoop   FORMAT ">>>>>>9"
   ldToday  FORMAT "99.99.9999"
   lcTime
   "PrepCDR Qty:" liCdrCount FORMAT ">>>>>>>9"
WITH
   ROW 6 WIDTH 60 CENTERED OVERLAY NO-LABEL
   TITLE " ActiveMQ PrepCDR Reader "
FRAME frmLog.

IF lcNagiosURL > "" THEN
   fKeepAlive("REPLOG_PREPCDR:PrepCDR Database Reader",lcNagiosURL).
PUT SCREEN ROW 1 COL 1 "NAGIOS: " + STRING(today,"99-99-9999") + " " +
                                    STRING(time,"hh:mm:ss").

RUN pInitialize(INPUT "amq_prepcdr").

IF RETURN-VALUE > "" THEN DO:
   IF LOG-MANAGER:LOGGING-LEVEL GE 1 THEN
      LOG-MANAGER:WRITE-MESSAGE(RETURN-VALUE, "ERROR").
   RETURN RETURN-VALUE.
END.

/* Call ActiveMQ Publisher class */
lMsgPublisher = NEW Gwy.MqPublisher(lcHost,liPort,
                                    liTimeOut,"hpd.prepcdr",
                                    lcUserName,lcPassword).

IF NOT VALID-OBJECT(lMsgPublisher) THEN DO:
   IF LOG-MANAGER:LOGGING-LEVEL GE 1 THEN
      LOG-MANAGER:WRITE-MESSAGE("ActiveMQ Publisher handle not found","ERROR").
END.

PrepCDR:
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
   "F8 TO QUIT, OTHER KEYS START PrepCDR READER IMMEDIATELLY".

   READKEY PAUSE liCdrFreq. /* the default is 120 (2 min.) */

   IF lcNagiosURL > "" THEN
      fKeepAlive("REPLOG_PREPCDR:PrepCDR Database Reader",lcNagiosURL).
   PUT SCREEN ROW 1 COL 1 "NAGIOS: " + STRING(today,"99-99-9999") + " " +
                                       STRING(time,"hh:mm:ss").
   IF KEYLABEL(LASTKEY) = "F8" THEN DO:
      ASSIGN llOk = FALSE.
      MESSAGE "ARE YOU SURE YOU WANT TO QUIT (Y/N) ? " UPDATE llOk.
      IF llOk THEN LEAVE PrepCDR.
   END.

END.

RUN pFinalize(INPUT "amq_prepcdr").

QUIT.

PROCEDURE pAmqCDRReader:

   DEFINE OUTPUT PARAMETER oiHandled AS INTEGER   NO-UNDO.

   ASSIGN ldaReadDate  = TODAY
          ldeReadInTS  = fMake2Dt(ldaReadDate,TIME)
          ldeCurrStamp = fMakeTS().

   DO TRANS:
      FIND FIRST ActionLog WHERE
                 ActionLog.Brand     = gcBrand        AND
                 ActionLog.ActionID  = "PrepCDR_HPD"   AND
                 ActionLog.TableName = "PrepCDR" EXCLUSIVE-LOCK NO-ERROR.
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
            ActionLog.TableName    = "PrepCDR"
            ActionLog.KeyValue     = "HPD"
            ActionLog.ActionID     = "PrepCDR_HPD"
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

   IF NOT CONNECTED("prepcdr") THEN
      RUN pDBConnect(ldaConnectDate).
   IF RETURN-VALUE BEGINS "ERROR" THEN RETURN.

   RUN pStartReader(ldaReadDate,
                    liReadTime,
                    ldeCurrStamp,
                    FALSE,
                    OUTPUT oiHandled,
                    INPUT-OUTPUT ldeCDRStamp).

   /* do two runs if db has been renewed on the event day -> some tickets have
      been saved to old db on the 1. day */
   ldaConnectDate = ?.
   FOR FIRST ttDB WHERE
             ttDb.ConnName = "" AND
             ttDb.TableName = "PrepCDR",
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
                          ldeCurrStamp,
                          TRUE,
                          OUTPUT oiHandled,
                          INPUT-OUTPUT ldeCDRStamp).
   END.

   DO TRANS:
      FIND FIRST ActionLog WHERE
                 ActionLog.Brand     = gcBrand AND
                 ActionLog.ActionID  = "PrepCDR_HPD" AND
                 ActionLog.TableName = "PrepCDR" EXCLUSIVE-LOCK NO-ERROR.
      IF AVAIL ActionLog THEN
         ActionLog.ActionDec = ldeCDRStamp.
      RELEASE ActionLog.
   END. /* DO TRANS: */

END PROCEDURE.

PROCEDURE pDBConnect:
 
   DEF INPUT PARAMETER idaConnectDate AS DATE NO-UNDO.
   
   /* connect to correct cdr dbs */
   fInitializeConnectTables("PrepCDR,McdrDtl2","").

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
   DEF INPUT  PARAMETER ideCurrStamp       AS DEC      NO-UNDO.
   DEF INPUT  PARAMETER llOldDB            AS LOG      NO-UNDO.
   DEF OUTPUT PARAMETER oiCount            AS INT      NO-UNDO.
   DEF INPUT-OUTPUT PARAMETER odeCDRStamp  AS DEC      NO-UNDO.

   DEF VAR lcDCEvent AS CHAR NO-UNDO.

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
      
      FOR EACH PrepCDR NO-LOCK USE-INDEX ReadDate WHERE
               PrepCDR.ReadDate = idaReadDate AND
               PrepCDR.ReadTime > iiReadTime
               BY ReadTime:

         IF PrepCDR.ErrorCode <> 0 THEN NEXT.

         IF PrepCDR.ReadInTS > ldeBufferTS THEN NEXT.

         ASSIGN lcMessage  = ""
                lcKeyValue = PrepCDR.CLI + lcDel2 + STRING(PrepCDR.DtlSeq) + lcDel2 +
                                                    STRING(PrepCDR.DateSt).

         IF PrepCDR.DCEvent > "" THEN
            lcDCEvent = PrepCDR.DCEvent.
         ELSE IF PrepCDR.BillCode = "PREMDUB" THEN
            lcDCEvent = "PMDUB".
         ELSE IF PrepCDR.CLIType = "TARJ7" AND PrepCDR.Charge = 0 THEN
            lcDCEvent = "TARJ7".
         ELSE IF PrepCDR.CLIType = "TARJ9" AND PrepCDR.EventType = "CALL" AND
                 PrepCDR.accumulator > 0 THEN
            lcDCEvent = "TARJ9".
         ELSE IF PrepCDR.CLIType = "TARJ9" AND PrepCDR.Charge = 0 AND
            LOOKUP(PrepCDR.GsmBnr,{&YOIGO_FREE_NUMBERS}) = 0 THEN
            lcDCEvent = "TARJ9". 
         ELSE lcDCEvent = "".

         lcMessage = 
            fNotNull("PrepCDR")                                           + lcDel +
            fNotNull("CREATE")                                            + lcDel +
            fNotNull(STRING(RECID(PrepCDR)))                              + lcDel +
            fNotNull(lcKeyValue)                                          + lcDel +
            fNotNull(STRING(ideCurrStamp))                                + lcDel +
            fNotNull(STRING(PrepCDR.MsSeq))                               + lcDel +
            fNotNull(STRING(PrepCDR.CLI))                                 + lcDel +
            fNotNull(PrepCDR.CLIType)                                     + lcDel +
            fNotNull(STRING(PrepCDR.InvCust))                             + lcDel +
            fNotNull(STRING(PrepCDR.DateSt,"99.99.9999"))                 + lcDel +
            fNotNull(STRING(PrepCDR.TimeSt,"HH:MM:SS"))                   + lcDel +
            fNotNull(STRING(PrepCDR.ReadInTS))                            + lcDel +
            fNotNull(PrepCDR.EventType)                                   + lcDel +
            fNotNull(PrepCDR.GsmBnr)                                      + lcDel +
            fNotNull(PrepCDR.BillCode)                                    + lcDel +
            fNotNull(STRING(PrepCDR.CCN))                                 + lcDel +
            fNotNull(STRING(PrepCDR.BillDur))                             + lcDel +
            fNotNull(STRING(PrepCDR.DataIn + PrepCDR.DataOut))            + lcDel +
            fNotNull(TRIM(STRING(PrepCDR.Charge,"->>>>>>>>>>>9.9<<<<<"))) + lcDel +
            fNotNull(lcDCEvent)                                           + lcDel +
            fNotNull(PrepCDR.BDest).

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
            odeCDRStamp = MAX(odeCDRStamp,PrepCDR.ReadinTS).

         /* Treshold value */
         IF oiCount >= 5000 THEN LEAVE Reader.

         CATCH anyError AS Progress.Lang.Error:
            LOG-MANAGER:WRITE-MESSAGE("Message failed was recovered: " + lcMessage,"DEBUG").
         END CATCH.

      END.

      idaReadDate = idaReadDate + 1.
   END.

END PROCEDURE.
