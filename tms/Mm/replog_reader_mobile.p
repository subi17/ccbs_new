/* ----------------------------------------------------------------------
  Module .......: Mm/replog_reader_mobile.p
  Task .........: Replication Log Reader for Mobile Database
  Application ..: TMS
  Author .......: Vikas
  Created ......: 24.05.13
  Version ......: Yoigo
---------------------------------------------------------------------- */
 
{commpaa.i}
katun = "Cron".
gcBrand = "1".
{timestamp.i}
{log.i}
{ftransdir.i}
{replog_reader.i}
{tmsconst.i}

FORM
   SKIP
   "ROUND:" liLoop   FORMAT ">>>>>>9"
   ldToday  FORMAT "99.99.9999"
   lcTime
   "RepLog Qty:" liRepLogs FORMAT ">>>>>>>9" 
WITH
   ROW 6 WIDTH 60 CENTERED OVERLAY NO-LABEL
   TITLE " Replication Logs for Mobile Database"
FRAME frmLog.

/* Initialize the configurations */

RUN pInitialize(INPUT "mobile").

IF RETURN-VALUE > "" THEN DO:
   IF LOG-MANAGER:LOGGING-LEVEL GE 1 THEN
      LOG-MANAGER:WRITE-MESSAGE(RETURN-VALUE, "ERROR").
   RETURN RETURN-VALUE.
END.

/* Call ActiveMQ Publisher class */
lMsgPublisher = NEW Gwy.MqPublisher(lcHost,liPort,
                                    liTimeOut,"hpd.mobile",
                                    lcUserName,lcPassword).

IF NOT VALID-OBJECT(lMsgPublisher) THEN DO:
   IF LOG-MANAGER:LOGGING-LEVEL GE 1 THEN
      LOG-MANAGER:WRITE-MESSAGE("ActiveMQ Publisher handle not found","ERROR").
END.

FUNCTION fCommonMessage RETURNS CHAR:

   DEF VAR lcMessage AS CHAR NO-UNDO.

   lcMessage = fNotNull(Mobile.RepLog.TableName)         + lcDel +
               fNotNull(Mobile.RepLog.EventType)         + lcDel +
               fNotNull(STRING(Mobile.RepLog.RecordId))  + lcDel +
               (IF Mobile.RepLog.TableName EQ "MsRequest"
                THEN fNotNull(ENTRY(1,Mobile.RepLog.KeyValue,CHR(255)))
                ELSE fNotNull(Mobile.RepLog.KeyValue))  + lcDel +
               fNotNull(STRING(Mobile.RepLog.EventTS)).

   RETURN lcMessage.
END. /* FUNCTION fCommonMessage RETURNS CHAR: */

/* MAIN BLOCK */

RepLog:
DO WHILE TRUE :

   liLoop = liLoop + 1.
   PAUSE 0.
   DISP  liLoop  
         Today @ ldToday 
         liRepLogs
         string(time,"hh:mm:ss") @ lctime
   WITH FRAME frmLog.
   PAUSE 0.

   RUN pHandleRepLog(OUTPUT liAmount).

   liRepLogs = liRepLogs + liAmount.

   /* Monitoring */
   IF lcNagiosURL > "" THEN
      fKeepAlive("REPLOG_MOBILE:Mobile Database Reader",lcNagiosURL).

   IF ldLogDate = ? THEN ldLogDate = TODAY.

   /* Replog handling statistics file */
   IF lcLogFileStat > "" AND ldLogDate <> TODAY THEN DO:

      lcStatLogFile = lcLogFileStat + "_mobile_" +
                      STRING(YEAR(ldLogDate)) + STRING(MONTH(ldLogDate),"99") +
                      STRING(DAY(ldLogDate),"99") + ".txt".

      OUTPUT STREAM sLogStat TO VALUE(lcStatLogFile).

      FOR EACH ttStat WHERE
               ttStat.ttDatabase = "Mobile" AND
               ttStat.ttDate     = ldLogDate EXCLUSIVE-LOCK:

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

RUN pFinalize(INPUT "mobile").

QUIT.

PROCEDURE pHandleRepLog:

   DEFINE OUTPUT PARAMETER oiHandled AS INTEGER   NO-UNDO.

   DEFINE VARIABLE llHandled         AS LOGICAL   NO-UNDO.
   DEFINE VARIABLE liHour            AS INTEGER   NO-UNDO.

   DEFINE BUFFER bRepLog FOR Mobile.RepLog.

   IF lcDumpSpool > "" AND
      (ldDate <> TODAY OR ldeTimeStamp <= fMakeTS()) THEN
      RUN pDumpFileRotation(INPUT "mobile").

   LOOP:
   FOR EACH Mobile.RepLog NO-LOCK BY EventTS:

      /* Treshold value */
      IF oiHandled >= 10000 THEN LEAVE LOOP.

      IF LOOKUP("pHandle" + RepLog.TableName,
                THIS-PROCEDURE:INTERNAL-ENTRIES) = 0 THEN NEXT.

      IF llLogFileAct THEN DO:
         OUTPUT STREAM sDump to VALUE(lcDumpFile).
         llLogFileAct = FALSE.
      END. /* IF llLogFileAct THEN DO: */

      DO TRANSACTION:
         FIND FIRST bRepLog WHERE
                    ROWID(bRepLog) = ROWID(RepLog)
              EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
         IF AVAIL bRepLog THEN DO:

            RUN VALUE("pHandle" + bRepLog.TableName) (OUTPUT llHandled).
            IF llHandled THEN DO:

               liHour = Time / 3600.

               FIND FIRST ttStat WHERE
                          ttStat.ttDatabase  = "Mobile"          AND
                          ttStat.ttTable     = bRepLog.TableName AND
                          ttStat.ttEventType = bRepLog.EventType AND
                          ttStat.ttDate      = TODAY             AND
                          ttStat.ttHour      = liHour EXCLUSIVE-LOCK NO-ERROR.
               IF AVAIL ttStat THEN
                  ttStat.ttEvents = ttStat.ttEvents + 1.
               ELSE DO:
                  CREATE ttStat.
                  ASSIGN ttStat.ttDatabase  = "Mobile"
                         ttStat.ttTable     = bRepLog.TableName
                         ttStat.ttEventType = bRepLog.EventType
                         ttStat.ttDate      = TODAY
                         ttStat.ttHour      = liHour
                         ttStat.ttEvents    = ttStat.ttEvents + 1.
               END. /* ELSE DO: */

               DELETE bRepLog.
               oiHandled = oiHandled + 1.
            END. /* IF llHandled THEN DO: */
         END. /* IF AVAIL bRepLog THEN DO: */
      END. /* DO TRANSACTION: */
   END. /* FOR EACH RepLog NO-LOCK: */

END PROCEDURE.

PROCEDURE pHandleDCCLI:

   DEFINE OUTPUT PARAMETER olHandled AS LOGICAL   NO-UNDO.

   DEFINE VARIABLE lcMessage         AS CHARACTER NO-UNDO.

   IF AVAIL Mobile.RepLog THEN DO:

      lcMessage = fCommonMessage().

      CASE RepLog.EventType:
         WHEN "CREATE" OR WHEN "MODIFY" THEN DO:
            FIND FIRST DCCLI WHERE
                       RECID(DCCLI) = RepLog.RecordId NO-LOCK NO-ERROR.
            IF AVAIL DCCLI THEN DO:
               lcMessage = lcMessage                             + lcDel +
                           fNotNull(STRING(DCCLI.MsSeq))         + lcDel +
                           fNotNull(STRING(DCCLI.PerContractID)) + lcDel +
                           fNotNull(DCCLI.DCEvent)               + lcDel +
                           fDateToString(DCCLI.ValidFrom)        + lcDel +
                           fDateToString(DCCLI.ValidTo)          + lcDel +
                           fDateToString(DCCLI.ValidToOrig)      + lcDel + 
                           fDateToString(DCCLI.RenewalDate)      + lcDel + 
                           fNotNull(STRING(DCCLI.Amount)).

               fWriteMessage(lcMessage).
            END.
            ELSE DO:
               olHandled = TRUE.
               fWriteMessage(lcMessage).
               RETURN.
            END.
         END.
         WHEN "DELETE" THEN fWriteMessage(lcMessage).
         OTHERWISE RETURN.
      END CASE.

      IF lMsgPublisher:send_message(lcMessage) THEN
         olHandled = TRUE.
      ELSE DO:
         olHandled = FALSE.
         IF LOG-MANAGER:LOGGING-LEVEL GE 1 THEN
            LOG-MANAGER:WRITE-MESSAGE("Message sending failed","ERROR").
      END.
   END.

   CATCH anyError AS Progress.Lang.Error:
      olHandled = FALSE.
      LOG-MANAGER:WRITE-MESSAGE("Message failed was recovered: " + lcMessage,"DEBUG").
   END CATCH.

END PROCEDURE.

PROCEDURE pHandleServPac:

   DEFINE OUTPUT PARAMETER olHandled AS LOGICAL   NO-UNDO.

   DEFINE VARIABLE lcMessage         AS CHARACTER NO-UNDO.

   IF AVAIL Mobile.RepLog THEN DO:

      lcMessage = fCommonMessage().

      CASE RepLog.EventType:
         WHEN "CREATE" OR WHEN "MODIFY" THEN DO:
            FIND FIRST ServPac WHERE
                       RECID(ServPac) = RepLog.RecordId NO-LOCK NO-ERROR.
            IF AVAIL ServPac THEN DO:
               lcMessage = lcMessage                 + lcDel +
                           fNotNull(ServPac.ServPac) + lcDel +
                           fNotNull(ServPac.SPName).

               fWriteMessage(lcMessage).
            END.
            ELSE DO:
               olHandled = TRUE.
               fWriteMessage(lcMessage).
               RETURN.
            END.
         END.
         WHEN "DELETE" THEN fWriteMessage(lcMessage).
         OTHERWISE RETURN.
      END CASE.

      IF lMsgPublisher:send_message(lcMessage) THEN
         olHandled = TRUE.
      ELSE DO:
         olHandled = FALSE.
         IF LOG-MANAGER:LOGGING-LEVEL GE 1 THEN
            LOG-MANAGER:WRITE-MESSAGE("Message sending failed","ERROR").
      END.
   END.

   CATCH anyError AS Progress.Lang.Error:
      olHandled = FALSE.
      LOG-MANAGER:WRITE-MESSAGE("Message failed was recovered: " + lcMessage,"DEBUG").
   END CATCH.

END PROCEDURE.

PROCEDURE pHandleIMSI:

   DEFINE OUTPUT PARAMETER olHandled AS LOGICAL   NO-UNDO.

   DEFINE VARIABLE lcMessage         AS CHARACTER NO-UNDO.

   IF AVAIL Mobile.RepLog THEN DO:

      lcMessage = fCommonMessage().

      CASE RepLog.EventType:
         WHEN "CREATE" OR WHEN "MODIFY" THEN DO:
            FIND FIRST IMSI WHERE
                       RECID(IMSI) = RepLog.RecordId NO-LOCK NO-ERROR.
            IF AVAIL IMSI THEN DO:
               lcMessage = lcMessage            + lcDel +
                           fNotNull(IMSI.IMSI)  + lcDel +
                           fNotNull(IMSI.ICC)   + lcDel +
                           fNotNull(IMSI.PUK1)  + lcDel +
                           fNotNull(IMSI.PUK2)  + lcDel +
                           fNotNull(IMSI.PIN1)  + lcDel +
                           fNotNull(IMSI.PIN2).

               fWriteMessage(lcMessage).
            END.
            ELSE DO:
               olHandled = TRUE.
               fWriteMessage(lcMessage).
               RETURN.
            END.
         END.
         WHEN "DELETE" THEN fWriteMessage(lcMessage).
         OTHERWISE RETURN.
      END CASE.

      IF lMsgPublisher:send_message(lcMessage) THEN
         olHandled = TRUE.
      ELSE DO:
         olHandled = FALSE.
         IF LOG-MANAGER:LOGGING-LEVEL GE 1 THEN
            LOG-MANAGER:WRITE-MESSAGE("Message sending failed","ERROR").
      END.
   END.

   CATCH anyError AS Progress.Lang.Error:
      olHandled = FALSE.
      LOG-MANAGER:WRITE-MESSAGE("Message failed was recovered: " + lcMessage,"DEBUG").
   END CATCH.

END PROCEDURE.

PROCEDURE pHandleDayCampaign:

   DEFINE OUTPUT PARAMETER olHandled AS LOGICAL   NO-UNDO.

   DEFINE VARIABLE lcMessage         AS CHARACTER NO-UNDO.
   DEFINE VARIABLE llDSS2Compatible  AS LOGICAL   NO-UNDO.

   IF AVAIL Mobile.RepLog THEN DO:

      lcMessage = fCommonMessage().

      CASE RepLog.EventType:
         WHEN "CREATE" OR WHEN "MODIFY" THEN DO:
            FIND FIRST DayCampaign WHERE
                       RECID(DayCampaign) = RepLog.RecordId NO-LOCK NO-ERROR.
            IF AVAIL DayCampaign THEN DO:

               /* TODO: remove hardcoding */
               llDSS2Compatible = DayCampaign.DCEvent EQ "CONT15" OR
                                  CAN-FIND(FIRST CLIType WHERE
                                                 CLIType.Brand    = gcBrand AND
                                                 CLIType.CLIType  = DayCampaign.DCEvent AND
                                                 CLIType.LineType > 0).

               lcMessage = lcMessage                                   + lcDel +
                           fNotNull(DayCampaign.DCEvent)               + lcDel +
                           fNotNull(DayCampaign.DCName)                + lcDel +
                           fNotNull(DayCampaign.DCType)                + lcDel +
                           fDateToString(DayCampaign.ValidFrom)        + lcDel +
                           fDateToString(DayCampaign.ValidTo)          + lcDel +
                           fNotNull(DayCampaign.FeeModel)              + lcDel +
                           fNotNull(DayCampaign.TermFeeModel)          + lcDel +
                           fNotNull(STRING(DayCampaign.DurMonths))     + lcDel +
                           fNotNull(STRING(DayCampaign.InstanceLimit)) + lcDel +
                           fNotNull(DayCampaign.BundleUpsell)          + lcDel +
                           fNotNull(STRING(DayCampaign.DSSPriority))   + lcDel +
                           fNotNull(STRING(llDSS2Compatible)).

               fWriteMessage(lcMessage).
            END.
            ELSE DO:
               olHandled = TRUE.
               fWriteMessage(lcMessage).
               RETURN.
            END.
         END.
         WHEN "DELETE" THEN fWriteMessage(lcMessage).
         OTHERWISE RETURN.
      END CASE.

      IF lMsgPublisher:send_message(lcMessage) THEN
         olHandled = TRUE.
      ELSE DO:
         olHandled = FALSE.
         IF LOG-MANAGER:LOGGING-LEVEL GE 1 THEN
            LOG-MANAGER:WRITE-MESSAGE("Message sending failed","ERROR").
      END.
   END.

   CATCH anyError AS Progress.Lang.Error:
      olHandled = FALSE.
      LOG-MANAGER:WRITE-MESSAGE("Message failed was recovered: " + lcMessage,"DEBUG").
   END CATCH.

END PROCEDURE.

PROCEDURE pHandleSubSer:

   DEFINE OUTPUT PARAMETER olHandled AS LOGICAL   NO-UNDO.

   DEFINE VARIABLE lcMessage         AS CHARACTER NO-UNDO.

   IF AVAIL Mobile.RepLog THEN DO:

      lcMessage = fCommonMessage().

      CASE RepLog.EventType:
         WHEN "CREATE" OR WHEN "MODIFY" THEN DO:
            FIND FIRST SubSer WHERE
                       RECID(SubSer) = RepLog.RecordId NO-LOCK NO-ERROR.
            IF AVAIL SubSer THEN DO:
               lcMessage = lcMessage                          + lcDel +
                           fNotNull(STRING(SubSer.MsSeq))     + lcDel +
                           fNotNull(SubSer.ServCom)           + lcDel +
                           fDateToString(SubSer.SSDate)       + lcDel +
                           fNotNull(STRING(SubSer.SSStat))    + lcDel +
                           fNotNull(SubSer.SSParam).

               fWriteMessage(lcMessage).
            END.
            ELSE DO:
               olHandled = TRUE.
               fWriteMessage(lcMessage).
               RETURN.
            END.
         END.
         WHEN "DELETE" THEN fWriteMessage(lcMessage).
         OTHERWISE RETURN.
      END CASE.

      IF lMsgPublisher:send_message(lcMessage) THEN
         olHandled = TRUE.
      ELSE DO:
         olHandled = FALSE.
         IF LOG-MANAGER:LOGGING-LEVEL GE 1 THEN
            LOG-MANAGER:WRITE-MESSAGE("Message sending failed","ERROR").
      END.
   END.

   CATCH anyError AS Progress.Lang.Error:
      olHandled = FALSE.
      LOG-MANAGER:WRITE-MESSAGE("Message failed was recovered: " + lcMessage,"DEBUG").
   END CATCH.

END PROCEDURE.


PROCEDURE pHandleMsRequest:

   DEFINE OUTPUT PARAMETER olHandled AS LOGICAL   NO-UNDO.

   DEFINE VARIABLE lcMessage         AS CHARACTER NO-UNDO.

   IF NOT AVAIL Mobile.RepLog THEN RETURN.

   lcMessage = fCommonMessage().

   CASE RepLog.EventType:
      WHEN "CREATE" OR WHEN "MODIFY" THEN DO:

         FIND MsRequest NO-LOCK WHERE
              RECID(MsRequest) = RepLog.RecordId NO-ERROR.

         IF AVAIL MsRequest THEN DO:
            IF LOOKUP(STRING(MsRequest.ReqType),{&REQTYPES_HPD}) > 0
            THEN DO:
               lcMessage = lcMessage + lcDel +
                           fNotNull(STRING(MsRequest.MsRequest)) + lcDel +
                           fNotNull(STRING(Msrequest.MsSeq)) + lcDel +
                           fNotNull(MsRequest.CLI) + lcDel +
                           fNotNull(STRING(MsRequest.Custnum)) + lcDel +
                           fNotNull(STRING(MsRequest.ReqType)) + lcDel +
                           fNotNull(STRING(MsRequest.ReqStatus)) + lcDel +
                           fNotNull(MsRequest.UserCode) + lcDel +
                           fNotNull(MsRequest.ReqSource) + lcDel +
                           fNotNull(STRING(MsRequest.CreStamp)) + lcDel +
                           fNotNull(STRING(MsRequest.ActStamp)) + lcDel +
                           fNotNull(STRING(MsRequest.DoneStamp)) + lcDel +
                           fNotNull(MsRequest.ReqCParam1) + lcDel +
                           fNotNull(MsRequest.ReqCParam2) + lcDel +
                           fNotNull(MsRequest.ReqCParam3) + lcDel +
                           fNotNull(MsRequest.ReqCParam4) + lcDel +
                           fNotNull(MsRequest.ReqCParam5) + lcDel +
                           fNotNull(STRING(MsRequest.ReqIParam1)) + lcDel +
                           fNotNull(STRING(MsRequest.ReqIParam2)) + lcDel +
                           fNotNull(STRING(MsRequest.ReqIParam3)) + lcDel +
                           fNotNull(STRING(MsRequest.ReqIParam4)) + lcDel +
                           fNotNull(STRING(MsRequest.ReqDParam1)).
               fWriteMessage(lcMessage).
            END.
            /* don't send to HPD, update request counters only */
            ELSE DO:
               olHandled = TRUE.
               RUN pUpdateRequestCounter.
               RETURN.
            END.
         END.
         ELSE DO:
            olHandled = TRUE.
            fWriteMessage(lcMessage).
            RETURN.
         END.
      END.
      WHEN "DELETE" THEN fWriteMessage(lcMessage).
      OTHERWISE RETURN.
   END CASE.

   IF lMsgPublisher:send_message(lcMessage) THEN DO:
      olHandled = TRUE.
      RUN pUpdateRequestCounter.
   END.
   ELSE DO:
      olHandled = FALSE.
      IF LOG-MANAGER:LOGGING-LEVEL GE 1 THEN
         LOG-MANAGER:WRITE-MESSAGE("Message sending failed","ERROR").
   END.

   CATCH anyError AS Progress.Lang.Error:
      olHandled = FALSE.
      LOG-MANAGER:WRITE-MESSAGE("Message failed was recovered: " + lcMessage,"DEBUG").
   END CATCH.

END PROCEDURE.

PROCEDURE pUpdateRequestCounter:
   
   DEFINE VARIABLE liReqType         AS INT NO-UNDO. 
   DEFINE VARIABLE liReqStatus       AS INT NO-UNDO. 
   DEFINE VARIABLE liReqStatusNew    AS INT NO-UNDO. 
   DEFINE VARIABLE llUpdateCounter   AS LOG NO-UNDO. 

   IF NUM-ENTRIES(Mobile.RepLog.KeyValue,CHR(255)) >= 3 THEN DO:
      ASSIGN
         liReqType   = INT(ENTRY(2,Mobile.RepLog.KeyValue,CHR(255)))
         liReqStatus = INT(ENTRY(3,Mobile.RepLog.KeyValue,CHR(255)))
         liReqStatusNew = INT(ENTRY(4,Mobile.RepLog.KeyValue,CHR(255))) WHEN
                       Mobile.RepLog.EventType EQ "MODIFY"
      NO-ERROR.
      IF ERROR-STATUS:ERROR THEN RETURN.

      IF Mobile.RepLog.EventType EQ "DELETE" OR
         Mobile.RepLog.EventType EQ "CREATE" THEN ASSIGN
            llUpdateCounter = TRUE
            liReqStatusNew = liReqStatus WHEN Mobile.RepLog.EventType EQ "CREATE".
      ELSE IF Mobile.RepLog.EventType EQ "MODIFY" AND
           liReqStatus NE liReqStatusNew
        THEN llUpdateCounter = TRUE.
   END.

   IF NOT llUpdateCounter THEN RETURN.

   /* old status counter update */
   IF Mobile.Replog.EventType EQ "DELETE" OR
      Mobile.Replog.EventType EQ "MODIFY" THEN DO:

      FIND FIRST MsReqStatistic EXCLUSIVE-LOCK WHERE
                 MsReqStatistic.Brand     = gcBrand AND
                 MsReqStatistic.ReqType   = liReqType AND 
                 MsReqStatistic.ReqStatus = liReqStatus NO-ERROR.

      IF AVAIL MsReqStatistic THEN
         MsReqStatistic.ReqStatusCount = MsReqStatistic.ReqStatusCount - 1.

      RELEASE MsReqStatistic.
   END.

   /* new status counter update */
   IF Mobile.Replog.EventType EQ "MODIFY" OR
      Mobile.Replog.EventType EQ "CREATE" THEN DO:

      FIND FIRST MsReqStatistic EXCLUSIVE-LOCK WHERE
                 MsReqStatistic.Brand     = gcBrand AND
                 MsReqStatistic.ReqType   = liReqType AND 
                 MsReqStatistic.ReqStatus = liReqStatusNew NO-ERROR.

      IF NOT AVAIL MsReqStatistic THEN DO:
         CREATE MsReqStatistic.
         ASSIGN
            MsReqStatistic.Brand = gcBrand
            MsReqStatistic.ReqType = liReqType
            MsReqStatistic.ReqStatus = liReqStatusNew
            MsReqStatistic.ReqStatusCount = 1.
      END.
      ELSE MsReqStatistic.ReqStatusCount = MsReqStatistic.ReqStatusCount + 1.

      RELEASE MsReqStatistic.
   END.
END PROCEDURE. 
