USING Progress.Lang.*.
USING HPD.DumpHandler.
USING HPD.DumpBase.
USING HPD.DumpFile.
USING HPD.DumpMQ.
USING HPD.DumpSomeWhere.

{HPD/HPDConst.i}

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

      IF icFile = "{&FileIDForMQ}"
      THEN objDumpBase = NEW DumpMQ(iiDumpID, THIS-OBJECT).
      ELSE objDumpBase = NEW DumpFile(iiDumpID, icFile, THIS-OBJECT).

      objDumpBase:mUseTable(BUFFER lbMServiceLimit:HANDLE).
      objDumpBase:mVerifyDumpFileData().

      mProcessFullDump(INPUT CAST(objDumpBase, DumpSomeWhere)).
      
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
              ELSE RETURN {HPD/keyvalue.i lbMServiceLimit . objDumpBase:lcKeyDelimiter MSID}.
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

      IF NOT AVAILABLE lbMServiceLimit
      THEN RETURN "".

      FIND CURRENT lbMServiceLimit NO-LOCK.

      IF lbMServiceLimit.MSID = 0 OR lbMServiceLimit.MSID = ?
      THEN DO:
         mLog(1,"INFO","MServiceLimit was ignored as the field MSID has zero value. ROWID=" + icRowID).
         RETURN ?.
      END.

      RETURN objDumpBase:mGetData().

   END METHOD.


   METHOD PRIVATE LOGICAL mCheckHPDStatus
      (iiMSSeq AS INTEGER,
       iiCustNum AS INTEGER):

      IF iiCustNum > 0
      THEN RETURN CAN-FIND(FIRST Customer NO-LOCK WHERE Customer.CustNum = iiCustNum).

      RETURN CAN-FIND(FIRST MobSub NO-LOCK WHERE MobSub.MsSeq = iiMsSeq).

   END METHOD.


   METHOD PRIVATE VOID mProcessFullDump
      (INPUT objDumpSomeWhere AS CLASS DumpSomeWhere):

      DEFINE VARIABLE ldeEndStamp AS DECIMAL NO-UNDO.
      DEFINE VARIABLE llSkip      AS LOGICAL NO-UNDO.
      
      ldeEndStamp = YEAR(TODAY) * 10000 + MONTH(TODAY) * 100 + 1.
   
      FOR
         EACH lbMServiceLimit NO-LOCK
         BREAK BY lbMServiceLimit.SlSeq
         ON QUIT UNDO, RETRY
         ON STOP UNDO, RETRY:

         IF FIRST-OF(lbMServiceLimit.SlSeq)
         THEN llSkip = FALSE.

         IF llSkip
         THEN NEXT.

         IF lbMServiceLimit.EndTS < ldeEndStamp
         THEN DO:
            llSkip = TRUE.
            NEXT.
         END.

         IF NOT mCheckHPDStatus(lbMServiceLimit.MsSeq, lbMServiceLimit.CustNum)
         THEN NEXT.
         
         liEvents = liEvents + 1.

         objDumpSomeWhere:mWriteALine().

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