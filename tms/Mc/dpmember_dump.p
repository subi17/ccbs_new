/* ----------------------------------------------------------------------
  MODULE .......: dpmember_dump.p
  TASK .........: Create a dump file for DPMembers
  APPLICATION ..: tms
  AUTHOR .......: aam 
  CREATED ......: 09.07.12
  Version ......: yoigo
---------------------------------------------------------------------- */

{commali.i}
{dumpfile_run.i}
{create_eventlog.i}

DEF INPUT  PARAMETER icDumpID      AS INT  NO-UNDO.
DEF INPUT  PARAMETER icFile        AS CHAR NO-UNDO.
DEF INPUT  PARAMETER icDumpMode    AS CHAR NO-UNDO.
DEF INPUT  PARAMETER idLastDump    AS DEC  NO-UNDO.
DEF INPUT  PARAMETER icEventSource AS CHAR NO-UNDO.
DEF INPUT  PARAMETER icEventFields AS CHAR NO-UNDO.
DEF OUTPUT PARAMETER oiEvents      AS INT  NO-UNDO.
DEF OUTPUT PARAMETER olInterrupted AS LOG  NO-UNDO.

DEF VAR lcNumeric    AS CHAR     NO-UNDO.
DEF VAR lcDelimiter  AS CHAR     NO-UNDO.
DEF VAR ldaModified  AS DATE     NO-UNDO.
DEF VAR liCnt        AS INT      NO-UNDO.
DEF VAR lcDumpFields AS CHAR     NO-UNDO.
DEF VAR lcValue      AS CHAR     NO-UNDO.
DEF VAR lcField      AS CHAR     NO-UNDO.
DEF VAR lcMSISDN     AS CHAR     NO-UNDO.
DEF VAR liKeyValue   AS INT      NO-UNDO.
DEF VAR lcUnit       AS CHAR     NO-UNDO.
DEF VAR lcMsSeq      AS CHAR     NO-UNDO. 
DEF VAR liDPId       AS INT      NO-UNDO. 
DEF VAR lcTable      AS CHAR     NO-UNDO. 
DEF VAR lcDataValues AS CHAR     NO-UNDO FORMAT "X(50)". 
DEF VAR lcItemFour AS CHAR NO-UNDO. 
DEF VAR lcItemFive AS CHAR NO-UNDO. 
DEF VAR lcItemSix AS CHAR NO-UNDO.
DEF VAR lcDiscValue AS CHAR NO-UNDO. 
DEF VAR lcValidFrom AS CHAR NO-UNDO. 
DEF VAR lcValidTo AS CHAR NO-UNDO.  
DEF VAR liFD AS INT NO-UNDO.
DEF VAR liFM AS INT NO-UNDO.
DEF VAR liFY AS INT NO-UNDO.
DEF VAR liTD AS INT NO-UNDO.
DEF VAR liTM AS INT NO-UNDO.
DEF VAR liTY AS INT NO-UNDO.
DEF VAR ldaValidFrom AS DATE NO-UNDO.
DEF VAR ldaValidTo AS DATE NO-UNDO.

DEF STREAM sFile.

DEF TEMP-TABLE ttDPMember NO-UNDO LIKE DPMember
   FIELD Action AS CHAR.

FUNCTION fCollectDPMember RETURNS LOGIC
   (INPUT icaction AS CHAR):

      IF NOT CAN-FIND( ttDPMember WHERE ttDPMember.DPId = DPMember.DPId AND
         ttDPMember.HostTable = DPMember.HostTable AND
         ttDPMember.KeyValue = DPMember.KeyValue) THEN DO:
      
         CREATE ttDPMember.
         BUFFER-COPY DPMember TO ttDPMember.
         ttDPMember.Action = icaction.
      END.

END FUNCTION. /* FUNCTION fCollectDPMember RETURNS LOGIC */

FUNCTION fCollectDPM_FromEventLog RETURNS LOGIC
   (INPUT icaction AS CHAR):

      IF NOT CAN-FIND( ttDPMember WHERE ttDPMember.DPId = liDPId AND
         ttDPMember.HostTable = lcTable AND
         ttDPMember.KeyValue = lcMsSeq) THEN DO:
      
         ASSIGN
            lcDataValues = REPLACE(EventLog.DataValues,CHR(255) + CHR(255),"|")
            lcItemFour = ENTRY(4,lcDataValues,"|")
            lcItemFive = ENTRY(5,lcDataValues,"|")
            lcItemSix = ENTRY(6,lcDataValues,"|")
            lcDiscValue = ENTRY(2,lcItemFour,CHR(255))
            lcValidFrom = ENTRY(2,lcItemFive,CHR(255))
            lcValidTo = ENTRY(2,lcItemSix,CHR(255))
            liFD = INT(ENTRY(3,lcValidFrom,"/"))
            liFM = INT(ENTRY(2,lcValidFrom,"/"))
            liFY = INT(ENTRY(1,lcValidFrom,"/"))
            liTD = INT(ENTRY(3,lcValidTo,"/"))
            liTM = INT(ENTRY(2,lcValidTo,"/"))
            liTY = INT(ENTRY(1,lcValidTo,"/"))
            ldaValidFrom = DATE(liFM,liFD,liFY)
            ldaValidTo = DATE(liTM,liTD,liTY).

         CREATE ttDPMember.
         ASSIGN
            ttDPMember.DiscValue = DEC(lcDiscValue)
            ttDPMember.DPId = liDPId
            ttDPMember.HostTable = lcTable
            ttDPMember.KeyValue = lcMsSeq
            ttDPMember.ValidFrom = ldaValidFrom
            ttDPMember.ValidTo = ldaValidTo
            ttDPMember.Action = icaction.
      END.

END FUNCTION. /* FUNCTION fCollectDPM_FromEventLog RETURNS LOGIC */

FUNCTION fMakeDPMemberDump RETURNS LOGIC:
   FOR EACH ttDPMember NO-LOCK:  
      FIND FIRST DiscountPlan NO-LOCK WHERE
                 DiscountPlan.DPId = ttDPMember.DPId.
         
      ASSIGN
         lcMSISDN   = ""
         liKeyValue = 0.
         
      liKeyValue = INT(ttDPMember.KeyValue) NO-ERROR.
      IF liKeyValue = 0 THEN NEXT. 
      
      IF ttDPMember.HostTable = "MobSub" THEN DO:
         FIND FIRST MsOwner WHERE MsOwner.MsSeq = liKeyValue NO-LOCK NO-ERROR.
         IF AVAILABLE MsOwner THEN lcMSISDN = MsOwner.CLI.
         ELSE NEXT.
      END.   

      CASE DiscountPlan.DPUnit:
      WHEN "Percentage" THEN lcUnit = "%".
      WHEN "Fixed" THEN lcUnit = "Eur".
      OTHERWISE lcUnit = "".
      END CASE. 
            
      DO liCnt = 1 TO NUM-ENTRIES(lcDumpFields):

         lcField = ENTRY(liCnt,lcDumpFields).
         
         IF lcField BEGINS "#" THEN DO:
            CASE lcField:
            WHEN "#HostTable" THEN lcValue = ttDPMember.HostTable.
            WHEN "#KeyValue"  THEN lcValue = ttDPMember.KeyValue.
            WHEN "#MSISDN"    THEN lcValue = lcMSISDN.
            WHEN "#DPRuleID"  THEN lcValue = DiscountPlan.DPRuleID.
            WHEN "#ValidFrom" THEN lcValue = STRING(ttDPMember.ValidFrom).
            WHEN "#ValidTo"   THEN lcValue = STRING(ttDPMember.ValidTo).
            WHEN "#DiscValue" THEN lcValue = STRING(ttDPMember.DiscValue).
            WHEN "#Unit"      THEN lcValue = lcUnit.   
            OTHERWISE lcValue = "".
            END CASE.
         END.

         PUT STREAM sFile UNFORMATTED
            lcValue.

         IF liCnt < NUM-ENTRIES(lcDumpFields) THEN 
         PUT STREAM sFile UNFORMATTED
            lcDelimiter.
      
      END.
      /* Add action information in the end of the line */
      PUT STREAM sFile UNFORMATTED lcDelimiter + ttDPMember.Action.

      PUT STREAM sFile UNFORMATTED
         SKIP.
      
      oiEvents = oiEvents + 1.

      IF NOT SESSION:BATCH AND oiEvents MOD 100 = 0 THEN DO:
         PAUSE 0. 
         DISP oiEvents LABEL "FATimes" 
         WITH OVERLAY ROW 10 CENTERED SIDE-LABELS
            TITLE " Collecting " FRAME fQty.
      END.
   END. /* FOR EACH ttDPMember NO-LOCK: */    
END FUNCTION. /* FUNCTION fMakeDPMemberDump RETURNS LOGIC: */                      


lcNumeric = SESSION:NUMERIC-FORMAT.

FIND FIRST DumpFile WHERE DumpFile.DumpID = icDumpID NO-LOCK NO-ERROR.
IF AVAILABLE DumpFile THEN DO:
   lcDelimiter  = fInitDelimiter(DumpFile.DumpDelimiter).
   
   IF DumpFile.DecimalPoint = "." 
   THEN SESSION:NUMERIC-FORMAT = "AMERICAN".
   ELSE SESSION:NUMERIC-FORMAT = "EUROPEAN".
END.
ELSE DO:
   ASSIGN 
      lcDelimiter = CHR(9)
      SESSION:NUMERIC-FORMAT = "AMERICAN".
END.

FOR EACH DFField OF DumpFile NO-LOCK WHERE
         DFField.ToDate   >= TODAY AND
         DFField.FromDate <= TODAY
BY DFField.OrderNbr:
   lcDumpFields = lcDumpFields + (IF lcDumpFields > "" THEN "," ELSE "") +
                  DFField.DFField.
END.
         
fSplitTS(idLastDump,
         OUTPUT ldaModified,
         OUTPUT liCnt).

OUTPUT STREAM sFile TO VALUE(icFile).



IF icDumpMode = "modified" THEN DO:
   /* Collect created discounts since last dump */
   FOR EACH DPMember NO-LOCK WHERE
            DPMember.ValidFrom = ldaModified
      ON QUIT UNDO, RETRY
      ON STOP UNDO, RETRY:

      IF RETRY THEN DO:
         olInterrupted = TRUE.
         LEAVE.
      END.
      fCollectDPMember('create').

   END.

   /* Collect modified or deleted discounts since last dump */
   FOR EACH EventLog NO-LOCK USE-INDEX TableName WHERE
            EventLog.TableName = "DPMember" AND
            EventLog.EventDate = ldaModified AND
            EventLog.Action <> "Create"
      ON QUIT UNDO, RETRY
      ON STOP UNDO, RETRY:

      IF RETRY THEN DO:
         olInterrupted = TRUE.
         LEAVE.
      END.
      liDPId = INT(ENTRY(1,EventLog.Key,CHR(255))).
      lcTable = ENTRY(2,EventLog.Key,CHR(255)).
      lcMsSeq = ENTRY(3,EventLog.Key,CHR(255)).

      IF EventLog.Action = "Modify" THEN DO:
         FOR EACH DPMember NO-LOCK WHERE
                  DPMember.DPId = liDPId AND
                  DPMember.HostTable = lcTable AND
                  DPMember.KeyValue = lcMsSeq.
            fCollectDPMember('modify').
         END.
      END.

      IF EventLog.Action = "Delete" THEN
            fCollectDPM_FromEventLog('delete').
  END.
END. /* modified dump */

ELSE DO:
   FOR EACH DPMember NO-LOCK
      ON QUIT UNDO, RETRY
      ON STOP UNDO, RETRY:

      IF RETRY THEN DO:
         olInterrupted = TRUE.
         LEAVE.
      END.
      fCollectDPMember('create').
   END.
END. /* full dump */


/* Make dump from ttDPMember */
fMakeDPMemberDump().

IF NOT SESSION:BATCH THEN 
   HIDE FRAME fQty NO-PAUSE.
 
OUTPUT STREAM sFile CLOSE.

SESSION:NUMERIC-FORMAT = lcNumeric.


