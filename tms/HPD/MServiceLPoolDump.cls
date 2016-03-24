USING Progress.Lang.*.
USING HPD.GenericDump.

ROUTINE-LEVEL ON ERROR UNDO, THROW.

CLASS HPD.MServiceLPoolDump FINAL INHERITS DumpHandler:

   DEFINE PRIVATE BUFFER lbMServiceLPool FOR MServiceLPool.
  
   CONSTRUCTOR PUBLIC MServiceLPoolDump
      (iiDumpID AS INTEGER):

      objDumpBase = NEW DumpBase(iiDumpID, THIS-OBJECT).
      objDumpBase:mUseTable(BUFFER lbMServiceLPool:HANDLE).
      objDumpBase:mVerifyDumpFileData().
      
   END CONSTRUCTOR.


   CONSTRUCTOR PUBLIC MServiceLPoolDump
      (iiDumpID    AS INTEGER,
       icFile      AS CHARACTER):

      objDumpBase = NEW DumpFile(iiDumpID, icFile, THIS-OBJECT).
      objDumpBase:mUseTable(BUFFER lbMServiceLPool:HANDLE).
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
              ELSE RETURN {HPD/keyvalue.i lbMServiceLPool . objDumpBase:lcKeyDelimiter MsSeq SLSeq EndTS}.
         OTHERWISE RETURN "".         
      END CASE.

   END METHOD.


   METHOD OVERRIDE PUBLIC CHARACTER mProcessRepLog
      (INPUT icRowID AS CHARACTER):

      FIND lbMServiceLPool NO-LOCK WHERE
         ROWID(lbMServiceLPool) = TO-ROWID(icRowID)
      NO-ERROR.

      IF AVAILABLE lbMServiceLPool
      THEN
      FOR
         FIRST MobSub FIELDS (MsSeq) NO-LOCK WHERE
            MobSub.MsSeq = lbMServiceLPool.MsSeq:
         RETURN objDumpBase:mGetData().
      END.

      RETURN "".

   END METHOD.


   METHOD PRIVATE VOID mProcessFullDump
      (INPUT objDumpFile AS CLASS DumpFile):

      /* We will dump only the newest ones */
      FOR
         EACH MobSub FIELDS (MsSeq) NO-LOCK,
         EACH lbMServiceLPool WHERE
            lbMServiceLPool.MsSeq  = MobSub.MsSeq
         BREAK BY lbMServiceLPool.SlSeq
         ON QUIT UNDO, RETRY
         ON STOP UNDO, RETRY:

         IF RETRY THEN DO:
            llInterrupted = TRUE.
            LEAVE.
         END.

         IF FIRST-OF(lbMServiceLPool.SlSeq)
         THEN DO:
            liEvents = liEvents + 1.

            objDumpFile:mWriteALine().

            IF NOT SESSION:BATCH AND liEvents MOD 100 = 0 THEN 
            DO:
               PAUSE 0.
               DISPLAY liEvents LABEL "MServiceLPool Counter" 
                  WITH OVERLAY ROW 10 CENTERED SIDE-LABELS
                  TITLE " Collecting " FRAME fQty.
            END.
         END.
      END.

      IF NOT SESSION:BATCH THEN
         HIDE FRAME fQty NO-PAUSE.

   END METHOD.

END CLASS.