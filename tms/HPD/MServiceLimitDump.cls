USING Progress.Lang.*.
USING HPD.DumpHandler.
USING HPD.DumpBase.
USING HPD.DumpFile.

ROUTINE-LEVEL ON ERROR UNDO, THROW.

CLASS HPD.MServiceLimitDump FINAL INHERITS DumpHandler:

   DEFINE PRIVATE BUFFER lbMServiceLimit FOR MServiceLimit.
  
   CONSTRUCTOR PUBLIC MServiceLimitDump
      (iiDumpID AS INTEGER):

      objDumpBase = NEW DumpBase(iiDumpID, THIS-OBJECT).
      objDumpBase:mUseTable(BUFFER lbMServiceLimit:HANDLE).
      objDumpBase:mVerifyDumpFileData().
      
   END CONSTRUCTOR.


   CONSTRUCTOR PUBLIC MServiceLimitDump
      (iiDumpID    AS INTEGER,
       icFile      AS CHARACTER):

      objDumpBase = NEW DumpFile(iiDumpID, icFile, THIS-OBJECT).
      objDumpBase:mUseTable(BUFFER lbMServiceLimit:HANDLE).
      objDumpBase:mVerifyDumpFileData().

      mProcessFullDump(INPUT CAST(objDumpBase, DumpFile)).
      
      /* If there has been problems we won't be here... */
      IF llInterrupted = FALSE
      THEN llAllOK = TRUE.
      
   END CONSTRUCTOR.


   METHOD PUBLIC OVERRIDE CHARACTER mSpecialHandling
      (icField     AS CHARACTER,
       ilCheckOnly AS LOGICAL):
   
      CASE icField:
         WHEN "KeyValue"
         THEN IF ilCheckOnly
              THEN RETURN "OK".
              ELSE RETURN {HPD/keyvalue.i lbMServiceLimit . objDumpBase:lcKeyDelimiter CustNum MsSeq DialType SLSeq EndTS}.
         WHEN "CustNum"
         THEN IF ilCheckOnly
              THEN RETURN "OK".
              ELSE RETURN IF lbMServiceLimit.CustNum = ? THEN "0" ELSE STRING(lbMServiceLimit.CustNum).         
         OTHERWISE RETURN "".         
      END CASE.

   END METHOD.


   METHOD OVERRIDE PUBLIC CHARACTER mProcessRepLog
      (INPUT icRowID AS CHARACTER):

      FIND lbMServiceLimit NO-LOCK WHERE
         ROWID(lbMServiceLimit) = TO-ROWID(icRowID)
      NO-ERROR.

      IF AVAILABLE lbMServiceLimit
      THEN
      FOR
         FIRST MobSub FIELDS (MsSeq) NO-LOCK WHERE
            MobSub.MsSeq = lbMServiceLimit.MsSeq:
         RETURN objDumpBase:mGetData().
      END.

      RETURN "".

   END METHOD.


   METHOD PRIVATE VOID mProcessFullDump
      (INPUT objDumpFile AS CLASS DumpFile):

      DEFINE VARIABLE lcGroupCode AS CHARACTER NO-UNDO.

      DEFINE VARIABLE ldeEndStamp AS DECIMAL NO-UNDO.
      
      ldeEndStamp = YEAR(TODAY) * 10000 + MONTH(TODAY) * 100 + 1.
   
      FOR
         EACH lbMServiceLimit NO-LOCK
         BREAK BY lbMServiceLimit.SlSeq
         ON QUIT UNDO, RETRY
         ON STOP UNDO, RETRY:

         IF FIRST-OF(lbMServiceLimit.SlSeq)
         THEN DO:
            lcGroupCode = "".
            FOR
               FIRST ServiceLimit FIELDS (SlSeq GroupCode) NO-LOCK WHERE
                  ServiceLimit.SlSeq = lbMServiceLimit.SlSeq:
               lcGroupCode = ServiceLimit.GroupCode.
            END.
         END.

         IF lbMServiceLimit.EndTS < ldeEndStamp
         THEN NEXT.

         IF lcGroupCode BEGINS "DSS" AND
            ( lbMServiceLimit.CustNum = 0 OR lbMServiceLimit.CustNum = ? OR  
              NOT CAN-FIND(FIRST Customer NO-LOCK WHERE Customer.CustNum = lbMServiceLimit.CustNum) )
         THEN NEXT.
         
         ELSE IF lbMServiceLimit.CustNum > 0
         THEN NEXT.

         IF NOT CAN-FIND(FIRST MobSub NO-LOCK WHERE MobSub.MsSeq = lbMServiceLimit.MsSeq)
         THEN NEXT.
         
         liEvents = liEvents + 1.

         objDumpFile:mWriteALine().

         IF NOT SESSION:BATCH AND liEvents MOD 100 = 0 THEN 
         DO:
            PAUSE 0.
            DISPLAY liEvents LABEL "MServiceLimit Counter" 
               WITH OVERLAY ROW 10 CENTERED SIDE-LABELS
               TITLE " Collecting " FRAME fQty.
         END.      
         
      END.

      IF NOT SESSION:BATCH THEN
         HIDE FRAME fQty NO-PAUSE.

   END METHOD.

END CLASS.