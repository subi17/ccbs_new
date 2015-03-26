/* ----------------------------------------------------------------------
  Module .......: Mm/replog_reader_common.p
  Task .........: Replication Log Reader for Common Database
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

FORM
   SKIP
   "ROUND:" liLoop   FORMAT ">>>>>>9"
   ldToday  FORMAT "99.99.9999"
   lcTime
   "RepLog Qty:" liRepLogs FORMAT ">>>>>>>9" 
WITH
   ROW 6 WIDTH 60 CENTERED OVERLAY NO-LABEL
   TITLE " Replication Logs for Common Database"
FRAME frmLog.

/* Initialize the configurations */

RUN pInitialize(INPUT "common").

IF RETURN-VALUE > "" THEN DO:
   IF LOG-MANAGER:LOGGING-LEVEL GE 1 THEN
      LOG-MANAGER:WRITE-MESSAGE(RETURN-VALUE, "ERROR").
   RETURN RETURN-VALUE.
END.

/* Call ActiveMQ Publisher class */
lMsgPublisher = NEW Gwy.MqPublisher(lcHost,liPort,
                                    liTimeOut,"hpd.common",
                                    lcUserName,lcPassword).

IF NOT VALID-OBJECT(lMsgPublisher) THEN DO:
   IF LOG-MANAGER:LOGGING-LEVEL GE 1 THEN
      LOG-MANAGER:WRITE-MESSAGE("ActiveMQ Publisher handle not found","ERROR").
END.

FUNCTION fCommonMessage RETURNS CHAR:

   DEF VAR lcMessage AS CHAR NO-UNDO.

   lcMessage = fNotNull(Common.RepLog.TableName)         + lcDel +
               fNotNull(Common.RepLog.EventType)         + lcDel +
               fNotNull(STRING(Common.RepLog.RecordId))  + lcDel +
               fNotNull(Common.RepLog.KeyValue)          + lcDel +
               fNotNull(STRING(Common.RepLog.EventTS)).

   RETURN lcMessage.
END. /* FUNCTION fCommonMessage RETURNS CHAR: */

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

   RUN pHandleRepLog(OUTPUT liAmount).

   liRepLogs = liRepLogs + liAmount.

   /* Monitoring */
   IF lcNagiosURL > "" THEN
      fKeepAlive("REPLOG_COMMON:Common Database Reader",lcNagiosURL).

   IF ldLogDate = ? THEN ldLogDate = TODAY.

   /* Replog handling statistics file */
   IF lcLogFileStat > "" AND ldLogDate <> TODAY THEN DO:

      lcStatLogFile = lcLogFileStat + "_common_" +
                      STRING(YEAR(ldLogDate)) + STRING(MONTH(ldLogDate),"99") +
                      STRING(DAY(ldLogDate),"99") + ".txt".

      OUTPUT STREAM sLogStat TO VALUE(lcStatLogFile).

      FOR EACH ttStat WHERE
               ttStat.ttDatabase  = "Common" AND
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

RUN pFinalize(INPUT "common").

QUIT.

PROCEDURE pHandleRepLog:

   DEFINE OUTPUT PARAMETER oiHandled AS INTEGER   NO-UNDO.

   DEFINE VARIABLE llHandled         AS LOGICAL   NO-UNDO.
   DEFINE VARIABLE liHour            AS INTEGER   NO-UNDO.

   DEFINE BUFFER bRepLog FOR Common.RepLog.

   /* Start Replog events dump file */
   IF lcDumpSpool > "" AND
      (ldDate <> TODAY OR ldeTimeStamp <= fMakeTS()) THEN
      RUN pDumpFileRotation(INPUT "common").

   LOOP:
   FOR EACH Common.RepLog NO-LOCK BY EventTs:

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
                          ttStat.ttDatabase  = "Common"          AND
                          ttStat.ttTable     = bRepLog.TableName AND
                          ttStat.ttEventType = bRepLog.EventType AND
                          ttStat.ttDate      = TODAY             AND
                          ttStat.ttHour      = liHour EXCLUSIVE-LOCK NO-ERROR.
               IF AVAIL ttStat THEN
                  ttStat.ttEvents = ttStat.ttEvents + 1.
               ELSE DO:
                  CREATE ttStat.
                  ASSIGN ttStat.ttDatabase  = "Common"
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

PROCEDURE pHandleBillItem:

   DEFINE OUTPUT PARAMETER olHandled AS LOGICAL   NO-UNDO.

   DEFINE VARIABLE lcMessage         AS CHARACTER NO-UNDO.

   IF AVAIL Common.RepLog THEN DO:

      lcMessage = fCommonMessage().

      CASE RepLog.EventType:
         WHEN "CREATE" OR WHEN "MODIFY" THEN DO:
            FIND FIRST BillItem WHERE
                       RECID(BillItem) = RepLog.RecordId NO-LOCK NO-ERROR.
            IF AVAIL BillItem THEN DO:
               lcMessage = lcMessage                    + lcDel +
                           fNotNull(BillItem.BillCode)  + lcDel +
                           fNotNull(BillItem.BIGroup)   + lcDel +
                           fNotNull(BillItem.BIName).
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

PROCEDURE pHandleBItemGroup:

   DEFINE OUTPUT PARAMETER olHandled AS LOGICAL   NO-UNDO.

   DEFINE VARIABLE lcMessage         AS CHARACTER NO-UNDO.

   IF AVAIL Common.RepLog THEN DO:

      lcMessage = fCommonMessage().

      CASE RepLog.EventType:
         WHEN "CREATE" OR WHEN "MODIFY" THEN DO:
            FIND FIRST BItemGroup WHERE
                       RECID(BItemGroup) = RepLog.RecordId NO-LOCK NO-ERROR.
            IF AVAIL BItemGroup THEN DO:
               lcMessage = lcMessage                     + lcDel +
                           fNotNull(BItemGroup.BIGroup)  + lcDel +
                           fNotNull(BItemGroup.BIGName).
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

PROCEDURE pHandleFMItem:

   DEFINE OUTPUT PARAMETER olHandled AS LOGICAL   NO-UNDO.

   DEFINE VARIABLE lcMessage         AS CHARACTER NO-UNDO.

   IF AVAIL Common.RepLog THEN DO:

      lcMessage = fCommonMessage().

      CASE RepLog.EventType:
         WHEN "CREATE" OR WHEN "MODIFY" THEN DO:
            FIND FIRST FMItem WHERE
                       RECID(FMItem) = RepLog.RecordId NO-LOCK NO-ERROR.
            IF AVAIL FMItem THEN DO:
               lcMessage = lcMessage                              + lcDel +
                           fNotNull(FMItem.FeeModel)              + lcDel +
                           fNotNull(FMItem.BillCode)              + lcDel +
                           fNotNull(STRING(FMItem.Amount))        + lcDel +
                           fNotNull(STRING(FMItem.FirstMonthBR)).
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

PROCEDURE pHandleServiceLimit:

   DEFINE OUTPUT PARAMETER olHandled AS LOGICAL   NO-UNDO.

   DEFINE VARIABLE lcMessage         AS CHARACTER NO-UNDO.

   IF AVAIL Common.RepLog THEN DO:

      lcMessage = fCommonMessage().

      CASE RepLog.EventType:
         WHEN "CREATE" OR WHEN "MODIFY" THEN DO:
            FIND FIRST ServiceLimit WHERE
                       RECID(ServiceLimit) = RepLog.RecordId NO-LOCK NO-ERROR.
            IF AVAIL ServiceLimit THEN DO:
               lcMessage = lcMessage                                + lcDel +
                           fNotNull(STRING(ServiceLimit.SLSeq))     + lcDel +
                           fNotNull(ServiceLimit.GroupCode)         + lcDel +
                           fNotNull(STRING(ServiceLimit.InclUnit))  + lcDel +
                           fNotNull(STRING(ServiceLimit.InclAmt))   + lcDel +
                           fDateToString(ServiceLimit.ValidFrom)    + lcDel +
                           fDateToString(ServiceLimit.ValidTo)      + lcDel +
                           fNotNull(STRING(ServiceLimit.DialType)).
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

PROCEDURE pHandleMServiceLimit:

   DEFINE OUTPUT PARAMETER olHandled AS LOGICAL   NO-UNDO.

   DEFINE VARIABLE lcMessage         AS CHARACTER NO-UNDO.

   IF AVAIL Common.RepLog THEN DO:

      lcMessage = fCommonMessage().

      CASE RepLog.EventType:
         WHEN "CREATE" OR WHEN "MODIFY" THEN DO:
            FIND FIRST MServiceLimit WHERE
                       RECID(MServiceLimit) = RepLog.RecordId NO-LOCK NO-ERROR.
            IF AVAIL MServiceLimit THEN DO:
               lcMessage = lcMessage                                + lcDel +
                           fNotNull(STRING(MServiceLimit.MSID))     + lcDel +
                           fNotNull(STRING(MServiceLimit.MsSeq))    + lcDel +
                           fNotNull(STRING(MServiceLimit.SLSeq))    + lcDel +
                           fNotNull(STRING(MServiceLimit.DialType)) + lcDel +
                           fNotNull(STRING(MServiceLimit.InclUnit)) + lcDel +
                           fNotNull(STRING(MServiceLimit.InclAmt))  + lcDel +
                           fNotNull(STRING(MServiceLimit.FromTS))   + lcDel +
                           fNotNull(STRING(MServiceLimit.EndTS))    + lcDel +
                           fNotNull(STRING(MServiceLimit.CustNum)).
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

PROCEDURE pHandleMServiceLPool:

   DEFINE OUTPUT PARAMETER olHandled AS LOGICAL   NO-UNDO.

   DEFINE VARIABLE lcMessage         AS CHARACTER NO-UNDO.

   IF AVAIL Common.RepLog THEN DO:

      lcMessage = fCommonMessage().

      CASE RepLog.EventType:
         WHEN "CREATE" OR WHEN "MODIFY" THEN DO:
            FIND FIRST MServiceLPool WHERE
                       RECID(MServiceLPool) = RepLog.RecordId NO-LOCK NO-ERROR.
            IF AVAIL MServiceLPool THEN DO:
               lcMessage = lcMessage                                + lcDel +
                           fNotNull(STRING(MServiceLPool.MsSeq))    + lcDel +
                           fNotNull(STRING(MServiceLPool.SLSeq))    + lcDel +
                           fNotNull(STRING(MServiceLPool.LimitAmt)) + lcDel +
                           fNotNull(STRING(MServiceLPool.FromTS))   + lcDel +
                           fNotNull(STRING(MServiceLPool.EndTS)).
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

PROCEDURE pHandleCustomerMServiceLPool:

   DEFINE OUTPUT PARAMETER olHandled AS LOGICAL   NO-UNDO.

   DEFINE VARIABLE lcMessage         AS CHARACTER NO-UNDO.

   IF AVAIL Common.RepLog THEN DO:

      lcMessage = fCommonMessage().

      CASE RepLog.EventType:
         WHEN "CREATE" OR WHEN "MODIFY" THEN DO:
            FIND FIRST MServiceLPool WHERE
                       RECID(MServiceLPool) = RepLog.RecordId NO-LOCK NO-ERROR.
            IF AVAIL MServiceLPool THEN DO:
               lcMessage = lcMessage                                + lcDel +
                           fNotNull(STRING(MServiceLPool.MsSeq))    + lcDel +
                           fNotNull(STRING(MServiceLPool.SLSeq))    + lcDel +
                           fNotNull(STRING(MServiceLPool.LimitAmt)) + lcDel +
                           fNotNull(STRING(MServiceLPool.FromTS))   + lcDel +
                           fNotNull(STRING(MServiceLPool.EndTS))    + lcDel +
                           fNotNull(STRING(MServiceLPool.CustNum)).
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

PROCEDURE pHandleCustomer:

   DEFINE OUTPUT PARAMETER olHandled AS LOGICAL   NO-UNDO.

   DEFINE VARIABLE lcMessage         AS CHARACTER NO-UNDO.

   IF AVAIL Common.RepLog THEN DO:

      lcMessage = fCommonMessage().

      CASE RepLog.EventType:
         WHEN "CREATE" OR WHEN "MODIFY" THEN DO:
            FIND FIRST Customer WHERE
                       RECID(Customer) = RepLog.RecordId NO-LOCK NO-ERROR.
            IF AVAIL Customer THEN DO:
               lcMessage = lcMessage                                + lcDel +
                           fNotNull(STRING(Customer.CustNum))       + lcDel +
                           fNotNull(Customer.CustId)                + lcDel +
                           fNotNull(Customer.OrgId)                 + lcDel +
                           fNotNull(Customer.FirstName)             + lcDel +
                           fNotNull(Customer.CustName)              + lcDel +
                           fNotNull(Customer.SurName2)              + lcDel +
                           fNotNull(Customer.ZipCode)               + lcDel +
                           fNotNull(Customer.Region)                + lcDel +
                           fNotNull(STRING(Customer.Language))      + lcDel +
                           fNotNull(STRING(Customer.DelType))       + lcDel +
                           fNotNull(Customer.CompanyName)           + lcDel +
                           fNotNull(Customer.Profession)            + lcDel +
                           fNotNull(Customer.Address)               + lcDel +
                           fNotNull(Customer.PostOffice)            + lcDel +
                           fNotNull(Customer.Country)               + lcDel +
                           fNotNull(Customer.Nationality)           + lcDel +
                           fNotNull(Customer.Email)                 + lcDel +
                           fNotNull(Customer.Phone)                 + lcDel +
                           fNotNull(Customer.SMSNumber)             + lcDel +
                           fNotNull(Customer.BankAcct).
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

PROCEDURE pHandleRepText:

   DEFINE OUTPUT PARAMETER olHandled AS LOGICAL   NO-UNDO.

   DEFINE VARIABLE lcMessage         AS CHARACTER NO-UNDO.

   IF AVAIL Common.RepLog THEN DO:

      lcMessage = fCommonMessage().

      CASE RepLog.EventType:
         WHEN "CREATE" OR WHEN "MODIFY" THEN DO:
            FIND FIRST RepText WHERE
                       RECID(RepText) = RepLog.RecordId NO-LOCK NO-ERROR.
            IF AVAIL RepText THEN DO:
               lcMessage = lcMessage                                + lcDel +
                           fNotNull(STRING(RepText.TextType))       + lcDel +
                           fNotNull(RepText.LinkCode)               + lcDel +
                           fNotNull(STRING(RepText.Language))       + lcDel +
                           fNotNull(REPLACE(RepText.RepText,CHR(10)," ")) + lcDel +
                           fDateToString(RepText.FromDate)          + lcDel +
                           fDateToString(RepText.ToDate).
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

PROCEDURE pHandleFixedFee:

   DEFINE OUTPUT PARAMETER olHandled AS LOGICAL   NO-UNDO.

   DEFINE VARIABLE lcMessage         AS CHARACTER NO-UNDO.

   IF AVAIL Common.RepLog THEN DO:

      lcMessage = fCommonMessage().

      CASE RepLog.EventType:
         WHEN "CREATE" OR WHEN "MODIFY" THEN DO:
            FIND FIRST FixedFee WHERE
                       RECID(FixedFee) = RepLog.RecordId NO-LOCK NO-ERROR.
            IF AVAIL FixedFee THEN DO:
               lcMessage = lcMessage                            + lcDel +
                           fNotNull(STRING(FixedFee.FFNum))     + lcDel +
                           fNotNull(STRING(FixedFee.CustNum))   + lcDel +
                           fNotNull(FixedFee.HostTable)         + lcDel +
                           fNotNull(FixedFee.KeyValue)          + lcDel +
                           fNotNull(FixedFee.FeeModel)          + lcDel +
                           fNotNull(FixedFee.BillCode)          + lcDel +
                           fNotNull(FixedFee.CalcObj)           + lcDel +
                           fNotNull(STRING(FixedFee.Amt))       + lcDel +
                           fNotNull(STRING(FixedFee.BegPeriod)) + lcDel +
                           fNotNull(STRING(FixedFee.EndPeriod)) + lcDel +
                           fNotNull(STRING(FixedFee.CustPP))    + lcDel +
                           fNotNull(STRING(FixedFee.CalcAmt))   + lcDel +
                           fNotNull(STRING(FixedFee.BegDate)).
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

PROCEDURE pHandleSingleFee:

   DEFINE OUTPUT PARAMETER olHandled AS LOGICAL   NO-UNDO.

   DEFINE VARIABLE lcMessage         AS CHARACTER NO-UNDO.

   IF AVAIL Common.RepLog THEN DO:

      lcMessage = fCommonMessage().

      CASE RepLog.EventType:
         WHEN "CREATE" OR WHEN "MODIFY" THEN DO:
            FIND FIRST SingleFee WHERE
                       RECID(SingleFee) = RepLog.RecordId NO-LOCK NO-ERROR.
            IF AVAIL SingleFee THEN DO:
               lcMessage = lcMessage                               + lcDel +
                           fNotNull(STRING(SingleFee.FMItemId))    + lcDel +
                           fNotNull(STRING(SingleFee.CustNum))     + lcDel +
                           fNotNull(SingleFee.HostTable)           + lcDel +
                           fNotNull(SingleFee.KeyValue)            + lcDel +
                           fNotNull(SingleFee.FeeModel)            + lcDel +
                           fNotNull(SingleFee.BillCode)            + lcDel +
                           fNotNull(SingleFee.CalcObj)             + lcDel +
                           fNotNull(STRING(SingleFee.Amt))         + lcDel +
                           fNotNull(STRING(SingleFee.BillPeriod))  + lcDel +
                           fNotNull(STRING(SingleFee.Concerns[1])) + lcDel +
                           fNotNull(SingleFee.SourceTable)         + lcDel +
                           fNotNull(SingleFee.SourceKey).
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

PROCEDURE pHandleInvoice:

   DEFINE OUTPUT PARAMETER olHandled AS LOGICAL   NO-UNDO.

   DEFINE VARIABLE lcMessage         AS CHARACTER NO-UNDO.

   IF AVAIL Common.RepLog THEN DO:

      lcMessage = fCommonMessage().

      CASE RepLog.EventType:
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

PROCEDURE pHandleRequestType:

   DEFINE OUTPUT PARAMETER olHandled AS LOGICAL   NO-UNDO.

   DEFINE VARIABLE lcMessage         AS CHARACTER NO-UNDO.

   IF NOT AVAIL Common.RepLog THEN RETURN.

   lcMessage = fCommonMessage().

   CASE RepLog.EventType:
      WHEN "CREATE" OR WHEN "MODIFY" THEN DO:
         FIND RequestType WHERE
              RECID(RequestType) = RepLog.RecordId NO-LOCK NO-ERROR.
         IF AVAIL RequestType THEN DO:
            lcMessage = lcMessage                             + lcDel +
                        fNotNull(STRING(RequestType.ReqType)) + lcDel +
                        fNotNull(RequestType.ReqName).
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

   CATCH anyError AS Progress.Lang.Error:
      olHandled = FALSE.
      LOG-MANAGER:WRITE-MESSAGE("Message failed was recovered: " + lcMessage,"DEBUG").
   END CATCH.

END PROCEDURE.

PROCEDURE pHandleDPMember:

   DEFINE OUTPUT PARAMETER olHandled AS LOGICAL   NO-UNDO.

   DEFINE VARIABLE lcMessage         AS CHARACTER NO-UNDO.

   IF AVAIL Common.RepLog THEN DO:

      lcMessage = fCommonMessage().

      CASE RepLog.EventType:
         WHEN "CREATE" OR WHEN "MODIFY" THEN DO:
            FIND FIRST DPMember WHERE
                       RECID(DPMember) = RepLog.RecordId NO-LOCK NO-ERROR.
            IF AVAIL DPMember THEN DO:
               lcMessage = lcMessage                            + lcDel +
                           fNotNull(STRING(DPMember.DPId))      + lcDel +
                           fNotNull(DPMember.HostTable)         + lcDel +
                           fNotNull(DPMember.KeyValue)          + lcDel + 
                           fNotNull(STRING(DPMember.DiscValue)) + lcDel + 
                           fNotNull(STRING(DPMember.ValidFrom)) + lcDel + 
                           fNotNull(STRING(DPMember.ValidTo)).
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

PROCEDURE pHandleDiscountPlan:

   DEFINE OUTPUT PARAMETER olHandled AS LOGICAL   NO-UNDO.

   DEFINE VARIABLE lcMessage         AS CHARACTER NO-UNDO.

   IF AVAIL Common.RepLog THEN DO:

      lcMessage = fCommonMessage().

      CASE RepLog.EventType:
         WHEN "CREATE" OR WHEN "MODIFY" THEN DO:
            FIND FIRST DiscountPlan WHERE
                       RECID(DiscountPlan) = RepLog.RecordId NO-LOCK NO-ERROR.
            IF AVAIL DiscountPlan THEN DO:
               lcMessage = lcMessage                                + lcDel +
                           fNotNull(STRING(DiscountPlan.DPId))      + lcDel +
                           fNotNull(STRING(DiscountPlan.ValidFrom)) + lcDel + 
                           fNotNull(STRING(DiscountPlan.ValidTo))   + lcDel + 
                           fNotNull(DiscountPlan.DPRuleID)          + lcDel +
                           fNotNull(DiscountPlan.DPName)            + lcDel +
                           fNotNull(STRING(DiscountPlan.DPUnit))    + lcDel + 
                           fNotNull(DiscountPlan.BillCode)          + lcDel +
                           fNotNull(STRING(DiscountPlan.ValidPeriods)).
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

PROCEDURE pHandleFATime:

   DEFINE OUTPUT PARAMETER olHandled AS LOGICAL   NO-UNDO.

   DEFINE VARIABLE lcMessage         AS CHARACTER NO-UNDO.

   IF AVAIL Common.RepLog THEN DO:

      lcMessage = fCommonMessage().

      CASE RepLog.EventType:
         WHEN "CREATE" OR WHEN "MODIFY" THEN DO:
            FIND FIRST FATime WHERE
                       RECID(FATime) = RepLog.RecordId NO-LOCK NO-ERROR.
            IF AVAIL Fatime THEN DO:
               lcMessage = lcMessage                        + lcDel +
                           fNotNull(STRING(FATime.FATNum))  + lcDel +
                           fNotNull(FATime.FTGrp)           + lcDel +
                           fNotNull(STRING(FATime.Period))  + lcDel +
                           fNotNull(STRING(FATime.Amt))     + lcDel +
                           fNotNull(FATime.CLI)             + lcDel +
                           fNotNull(STRING(FATime.MsSeq))   + lcDel +
                           fNotNull(STRING(FATime.CustNum)) + lcDel + 
                           fNotNull(STRING(FATime.Used))    + lcDel + 
                           fNotNull(STRING(FATime.InvNum)).
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

PROCEDURE pHandleFATGroup:

   DEFINE OUTPUT PARAMETER olHandled AS LOGICAL   NO-UNDO.

   DEFINE VARIABLE lcMessage         AS CHARACTER NO-UNDO.

   IF AVAIL Common.RepLog THEN DO:

      lcMessage = fCommonMessage().

      CASE RepLog.EventType:
         WHEN "CREATE" OR WHEN "MODIFY" THEN DO:
            FIND FIRST FATGroup WHERE
                       RECID(FATGroup) = RepLog.RecordId NO-LOCK NO-ERROR.
            IF AVAIL FATGroup THEN DO:
               lcMessage = lcMessage                           + lcDel +
                           fNotNull(FATGroup.FTGrp)            + lcDel +
                           fNotNull(FATGroup.FtgName)          + lcDel +
                           fNotNull(STRING(FATGroup.BillCode)) + lcDel +
                           fNotNull(STRING(FATGroup.Amt)).
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

PROCEDURE pHandleNatHoliday:

   DEFINE OUTPUT PARAMETER olHandled AS LOGICAL   NO-UNDO.

   DEFINE VARIABLE lcMessage         AS CHARACTER NO-UNDO.

   IF AVAIL Common.RepLog THEN DO:

      lcMessage = fCommonMessage().

      CASE RepLog.EventType:
         WHEN "CREATE" OR WHEN "MODIFY" THEN DO:
            FIND FIRST NatHoliday WHERE
                       RECID(NatHoliday) = RepLog.RecordId NO-LOCK NO-ERROR.
            IF AVAIL NatHoliday THEN DO:
               lcMessage = lcMessage                            + lcDel +
                           fNotNull(STRING(NatHoliday.Holiday)) + lcDel +
                           fNotNull(NatHoliday.HName).
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
