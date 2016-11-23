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
DEF VAR ldaActDate   AS DATE NO-UNDO.
DEF VAR liActTime    AS INT  NO-UNDO.

ASSIGN
   ldaStartDate = DATE(MONTH(TODAY), 1, YEAR(TODAY))
   ldaEndDate   = DATE(MONTH(TODAY), 3, YEAR(TODAY))
   ldStartTime  = fMake2Dt(ldaStartDate,0)
   ldEndTime    = fMake2Dt(ldaEndDate  ,86399).

MESSAGE "Create Duplicate Invoice Request ?" SKIP
        "For Creating - YES" SKIP
        "For Log File - NO " VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO-CANCEL
        TITLE "Duplicate Invoice Request" UPDATE llConfirm AS LOGICAL.

OUTPUT STREAM sOut TO "ydr_2348_lastinvoices_test_final.log".

PUT STREAM sOut UNFORMATTED
   "SubscriptionID;MSISDN;CustomerNumber;EventDate;EventTime"
   SKIP.

FOR EACH EventLog NO-LOCK WHERE
         EventLog.UserCode  = "TermSub"                  AND
         EventLog.TableName = "Customer"                 AND
         EventLog.Action    = "Modify"                   AND
         EventLog.EventDate >= ldaStartDate              AND
         EventLog.EventDate <= ldaEndDate                AND
         LOOKUP("DelType",EventLog.Datavalues,CHR(255)) > 0:

   IF CAN-FIND(FIRST MobSub NO-LOCK WHERE
                     MobSub.Brand   = "1"               AND
                     MobSub.CustNum = INT(EventLog.Key) AND 
                     MobSub.PayType EQ FALSE)           THEN NEXT.

   ASSIGN
      liOldDelType = INT(ENTRY(2,EventLog.Datavalues,CHR(255)))
      liNewDelType = INT(ENTRY(3,EventLog.Datavalues,CHR(255))).
      
   IF (liOldDelType = 2 OR liOldDelType = 4) AND
      liNewDelType = 1 THEN
   DO:

      IF NOT CAN-FIND(FIRST ttCust WHERE
                            ttCust.CustNum = INT(EventLog.Key)) THEN
      DO:
         CREATE ttCust.
         ASSIGN ttCust.CustNum = INT(EventLog.Key) 
                ttCust.EndTS   = fHMS2TS(EventLog.EventDate, 
                                         EventLog.EventTime).
                                         
         PUT STREAM sOut UNFORMATTED
            EventLog.Key                      ";"
            STRING(EventLog.EventDate,"99.99.9999") ";"
            EventLog.EventTime
            SKIP.

         iCount = iCount + 1.
         DISP iCount.
         PAUSE 0.
      END.
   END.

END.

OUTPUT STREAM sOut CLOSE.

DEFINE VARIABLE i AS INTEGER NO-UNDO. 

OUTPUT STREAM sOut To "ydr_2348_duplicate_invoice_requests_final.txt" append.
IF llConfirm THEN
FOR EACH ttCust:
   FIND FIRST Invoice NO-LOCK WHERE
              Invoice.Brand     = "1"            AND
              Invoice.CustNum   = ttCust.CustNum AND
              Invoice.InvDate   = ldaStartDate   AND
              Invoice.InvType   = 1              AND
              Invoice.ChgStamp <= ttCust.EndTS   NO-ERROR.
   IF AVAIL Invoice THEN DO:
      IF CAN-FIND(FIRST MsRequest NO-LOCK WHERE
                        MsRequest.Brand      = "1"                          AND
                        MsRequest.ReqType    = {&REQTYPE_DUPLICATE_INVOICE} AND
                        MsRequest.CustNum    = ttCust.CustNum               AND
                        MsRequest.ReqIParam1 = Invoice.InvNum               AND
                        LOOKUP(STRING(MsRequest.ReqStatus),{&REQ_INACTIVE_STATUSES}) = 0) THEN NEXT.
      
      CREATE MsRequest.
      ASSIGN
         MsRequest.MsRequest  = NEXT-VALUE(MsRequest)
         MsRequest.ReqType    = {&REQTYPE_DUPLICATE_INVOICE}
         MsRequest.Brand      = "1"
         MsRequest.UserCode   = "Qvantel"
         MsRequest.ActStamp   = fMake2Dt(11/10/2016,0) 
         MsRequest.ReqStatus  = 0
         MsRequest.CustNum    = ttCust.CustNum
         MsRequest.CreStamp   = fMakeTS()
         MsRequest.ReqIParam1 = Invoice.InvNum
         MsRequest.ReqCParam1 = Invoice.ExtInvID
         MsRequest.ReqSource  = {&REQUEST_SOURCE_SCRIPT}
         MsRequest.Memo       = "YDR-2050".

      PUT STREAM sOut UNFORMATTED
         MsRequest.MsRequest   ";"
          MsRequest.CustNum    ";"
          MsRequest.ReqIParam1 ";"
          MsRequest.ReqCParam1 SKIP.
      RELEASE MsRequest. 

      i = i + 1.

      if i mod 10 eq 0 then do:
         disp i. 
         pause 0.
      END.

     /* PUT STREAM sOut UNFORMATTED
          Invoice.CustNum    ";"
          Invoice.ExtInvId   ";"
          Invoice.InvDate    ";"
          Invoice.ChgStamp   ";"
          ttCust.EndTs       ";"
          Invoice.PrintState ";"
          Invoice.DelType    SKIP.  */
   END.
END.

OUTPUT STREAM sOut CLOSE.
