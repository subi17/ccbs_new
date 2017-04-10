USING Progress.Lang.*.
USING HPD.DumpHandler.
USING HPD.DumpBase.
USING HPD.DumpFile.
USING HPD.DumpMQ.
USING HPD.DumpSomeWhere.

{HPD/HPDConst.i}

ROUTINE-LEVEL ON ERROR UNDO, THROW.

CLASS HPD.MServiceLPoolDump FINAL INHERITS DumpHandler:

   DEFINE PRIVATE BUFFER lbMServiceLPool FOR MServiceLPool.
  
   CONSTRUCTOR PUBLIC MServiceLPoolDump
      (iiDumpID AS INTEGER):

      lhMainTable = BUFFER lbMServiceLPool:HANDLE.

      objDumpBase = NEW DumpBase(iiDumpID, THIS-OBJECT).
      objDumpBase:mUseTable(lhMainTable).
      objDumpBase:mVerifyDumpFileData().
      
   END CONSTRUCTOR.


   CONSTRUCTOR PUBLIC MServiceLPoolDump
      (iiDumpID    AS INTEGER,
       icFile      AS CHARACTER):

      IF icFile = "{&FileIDForMQ}"
      THEN objDumpBase = NEW DumpMQ(iiDumpID, THIS-OBJECT).
      ELSE objDumpBase = NEW DumpFile(iiDumpID, icFile, THIS-OBJECT).

      objDumpBase:mUseTable(BUFFER lbMServiceLPool:HANDLE).
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
              ELSE RETURN {HPD/keyvalue.i lbMServiceLPool . objDumpBase:lcKeyDelimiter CustNum MsSeq SLSeq EndTS}.
         OTHERWISE RETURN "".         
      END CASE.

   END METHOD.


   METHOD PRIVATE VOID mProcessFullDump
      (INPUT objDumpSomeWhere AS CLASS DumpSomeWhere):

      DEFINE VARIABLE ldeEndStamp AS DECIMAL NO-UNDO.
      DEFINE VARIABLE llSkip      AS LOGICAL NO-UNDO.
      DEFINE VARIABLE ldaPickFrom AS DATE    NO-UNDO.

      /* prepaid packet can be active also in the previous month */
      ASSIGN
         ldaPickFrom = ADD-INTERVAL(TODAY, -1, "months")
         ldeEndStamp = YEAR(ldaPickFrom) * 10000 +
                       MONTH(ldaPickFrom) * 100 +
                       DAY(ldaPickFrom).

      DO WITH FRAME fQty:

         /* Dump subscription's MServiceLPools */
         FOR
            EACH lbMServiceLPool NO-LOCK USE-INDEX MSSeq
            BREAK
               BY lbMServiceLPool.MsSeq
               BY lbMServiceLPool.SlSeq
            ON QUIT UNDO, RETRY
            ON STOP UNDO, RETRY:

            IF RETRY THEN DO:
               llInterrupted = TRUE.
               LEAVE.
            END.

            IF FIRST-OF(lbMServiceLPool.MsSeq)
            THEN DO:
               IF CAN-FIND(FIRST MobSub NO-LOCK WHERE
                                 MobSub.MsSeq = lbMServiceLPool.MsSeq)
               THEN llSkip = FALSE.
               ELSE llSkip = TRUE.
            END.

            IF llSkip THEN NEXT.

            IF lbMServiceLPool.EndTS < ldeEndStamp THEN NEXT.

            liEvents = liEvents + 1.

            objDumpSomeWhere:mWriteALine().

            IF NOT SESSION:BATCH AND liEvents MOD 100 = 0 THEN
            DO:
               PAUSE 0.
               DISPLAY liEvents LABEL "MServiceLPool Counter"
                  WITH OVERLAY ROW 10 CENTERED SIDE-LABELS
                  TITLE " Collecting " FRAME fQty.
            END.
         END.

         /* Dump customer's MServiceLPools */
         FOR
            EACH lbMServiceLPool NO-LOCK USE-INDEX CustNum
            BREAK
               BY lbMServiceLPool.CustNum
               BY lbMServiceLPool.SlSeq
            ON QUIT UNDO, RETRY
            ON STOP UNDO, RETRY:

            IF RETRY THEN DO:
               llInterrupted = TRUE.
               LEAVE.
            END.

            IF FIRST-OF(lbMServiceLPool.CustNum)
            THEN DO:
               IF CAN-FIND(FIRST Customer NO-LOCK WHERE
                                 Customer.CustNum = lbMServiceLPool.CustNum)
               THEN llSkip = FALSE.
               ELSE llSkip = TRUE.
            END.

            IF llSkip THEN NEXT.

            IF lbMServiceLPool.EndTS < ldeEndStamp THEN NEXT.

            liEvents = liEvents + 1.

            objDumpSomeWhere:mWriteALine().

            IF NOT SESSION:BATCH AND liEvents MOD 100 = 0 THEN
            DO:
               PAUSE 0.
               DISPLAY liEvents LABEL "MServiceLPool Counter"
                  WITH OVERLAY ROW 10 CENTERED SIDE-LABELS
                  TITLE " Collecting " FRAME fQty.
            END.
         END.

         IF NOT SESSION:BATCH THEN
            HIDE FRAME fQty NO-PAUSE.

      END.

   END METHOD.

END CLASS.
