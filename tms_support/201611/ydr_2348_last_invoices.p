{tmsconst.i}
{timestamp.i}

DEF STREAM sOut.

DEF TEMP-TABLE ttCust NO-UNDO
   FIELD CustNum AS INT
   FIELD EndTS   AS DEC
   INDEX CustNum CustNum
   .

DEF VAR iCount       AS INT  NO-UNDO.
DEF VAR liOldDelType AS INT  NO-UNDO.
DEF VAR liNewDelType AS INT  NO-UNDO.
DEF VAR ldStartTime  AS DEC  NO-UNDO.
DEF VAR ldEndTime    AS DEC  NO-UNDO.
DEF VAR ldaStartDate AS DATE NO-UNDO. 
DEF VAR ldaEndDate   AS DATE NO-UNDO. 

ASSIGN
   ldaStartDate = DATE(MONTH(TODAY), 1, YEAR(TODAY))
   ldaEndDate   = DATE(MONTH(TODAY), 3, YEAR(TODAY))
   ldStartTime  = fMake2Dt(ldaStartDate,0)
   ldEndTime    = fMake2Dt(ldaEndDate  ,86399).

MESSAGE "Create Duplicate Invoice Request ?" SKIP
        "For Creating - YES" SKIP
        "For Log File - NO" VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO-CANCEL
        TITLE "Duplicate Invoice Request" UPDATE llConfirm AS LOGICAL.

OUTPUT STREAM sOut TO "ydr_2348_lastinvoices.log".

PUT STREAM sOut UNFORMATTED
   "SubscriptionID;MSISDN;CustomerNumber;EventDate;EventTime"
   SKIP.

FOR EACH MsRequest NO-LOCK WHERE
         MsRequest.Brand     = "1"         AND
         MsRequest.ReqType   = 18          AND
         MsRequest.ReqStatus = 2           AND
         MsRequest.ActStamp >= ldStartTime AND
         MsRequest.ActStamp <= ldEndTime:

   IF CAN-FIND(FIRST MobSub NO-LOCK WHERE
                     MobSub.Brand = "1"           AND
                     MobSub.MsSeq = MsRequest.MsSeq) THEN NEXT.

   FIND FIRST TermMobSub NO-LOCK WHERE
              TermMobSub.Brand = "1"             AND
              TermMobSub.MsSeq = MsRequest.MsSeq NO-ERROR.
   IF AVAIL TermMobSub THEN DO:
 
      IF CAN-FIND(FIRST MobSub NO-LOCK WHERE
                        MobSub.Brand    = "1"                 AND
                        MobSub.CustNum  = TermMobSub.CustNum  AND
                        MobSub.MsSeq   <> TermMobSub.MsSeq    AND
                        MobSub.PayType  = NO)                 THEN NEXT.

      FOR EACH EventLog NO-LOCK WHERE
               EventLog.UserCode  = "TermSub"                  AND
               EventLog.TableName = "Customer"                 AND
               EventLog.Key       = STRING(TermMobSub.CustNum) AND
               EventLog.Action    = "Modify"                   AND
               EventLog.EventDate >= ldaStartDate              AND
               Eventlog.EventDate <= ldaEndDate                AND
               EventLog.EventTime <> ""                        AND
               LOOKUP("DelType",EventLog.Datavalues,CHR(255)) > 0:

         ASSIGN
            liOldDelType = INT(ENTRY(2,EventLog.Datavalues,CHR(255)))
            liNewDelType = INT(ENTRY(3,EventLog.Datavalues,CHR(255))).

         IF (liOldDelType = 2 OR liOldDelType = 4) AND
            liNewDelType = 1 THEN
         DO:

            IF NOT CAN-FIND(FIRST ttCust WHERE
                                  ttCust.CustNum = TermMobSub.CustNum) THEN
            DO:
               FIND FIRST MsOwner NO-LOCK WHERE
                          MsOwner.MsSeq = TermMobSub.MsSeq NO-ERROR.
               CREATE ttCust.
               ASSIGN ttCust.CustNum = TermMobSub.CustNum
                      ttCust.EndTS   = MsOwner.TsEnd.

               PUT STREAM sOut UNFORMATTED
                  TermMobSub.MsSeq                        ";"
                  TermMobSub.CLI                          ";"
                  TermMobSub.CustNum                      ";"
                  STRING(EventLog.EventDate,"99.99.9999") ";"
                  EventLog.EventTime
                  SKIP.

               iCount = iCount + 1.
               DISP iCount.
               PAUSE 0.
            END.
         END.
      END.
   END.
END.

OUTPUT STREAM sOut CLOSE.

IF llConfirm THEN
FOR EACH ttCust:
   FIND FIRST Invoice NO-LOCK WHERE
              Invoice.Brand     = "1"            AND
              Invoice.CustNum   = ttCust.CustNum AND
              Invoice.InvDate   = ldaStartDate   AND
              Invoice.ChgStamp <= ttCust.EndTS   NO-ERROR.
   IF AVAIL Invoice THEN DO:
      CREATE MsRequest.
      ASSIGN 
         MsRequest.MsRequest  = NEXT-VALUE(MsRequest)
         MsRequest.ReqType    = {&REQTYPE_DUPLICATE_INVOICE}
         MsRequest.Brand      = "1"
         MsRequest.UserCode   = "YDR-2348"
         MsRequest.ActStamp   = fMakeTS()
         MsRequest.ReqStatus  = 0
         MsRequest.CustNum    = ttCust.CustNum
         MsRequest.CreStamp   = fMakeTS()
         MsRequest.ReqIParam1 = Invoice.InvNum
         MsRequest.ReqCParam1 = Invoice.ExtInvID
         MsRequest.ReqSource  = {&REQUEST_SOURCE_SCRIPT}
         .
      RELEASE MsRequest. 
   END.
END.
