USING Progress.Lang.*.
USING HPD.DumpHandler.
USING HPD.DumpBase.
USING HPD.DumpFile.

ROUTINE-LEVEL ON ERROR UNDO, THROW.

CLASS HPD.SingleFeeDump FINAL INHERITS DumpHandler:

   DEFINE PRIVATE BUFFER lbSingleFee FOR SingleFee.
  
   CONSTRUCTOR PUBLIC SingleFeeDump
      (iiDumpID AS INTEGER):

      objDumpBase = NEW DumpBase(iiDumpID, THIS-OBJECT).
      objDumpBase:mUseTable(BUFFER lbSingleFee:HANDLE).
      objDumpBase:mVerifyDumpFileData().
      
   END CONSTRUCTOR.


   CONSTRUCTOR PUBLIC SingleFeeDump
      (iiDumpID    AS INTEGER,
       icFile      AS CHARACTER):

      objDumpBase = NEW DumpFile(iiDumpID, icFile, THIS-OBJECT).
      objDumpBase:mUseTable(BUFFER lbSingleFee:HANDLE).
      objDumpBase:mVerifyDumpFileData().

      mProcessFullDump(INPUT CAST(objDumpBase, DumpFile)).
      
      /* If there has been problems we won't be here... */
      IF llInterrupted = FALSE
      THEN llAllOK = TRUE.
      
   END CONSTRUCTOR.


   METHOD OVERRIDE PUBLIC CHARACTER mProcessRepLog
      (INPUT icRowID AS CHARACTER):

      FIND lbSingleFee NO-LOCK WHERE
         ROWID(lbSingleFee) = TO-ROWID(icRowID)
      NO-ERROR.

      IF AVAILABLE lbSingleFee
      THEN DO:
          IF lbSingleFee.HostTable NE "MobSub"
          THEN RETURN "".
          FOR
             FIRST MobSub FIELDS (MsSeq) NO-LOCK WHERE
                MobSub.MsSeq = INTEGER(lbSingleFee.KeyValue):
             RETURN objDumpBase:mGetData().
          END.
      END.

      RETURN "".

   END METHOD.


   METHOD PRIVATE VOID mProcessFullDump
      (INPUT objDumpFile AS CLASS DumpFile):

      DEFINE VARIABLE liPeriod  AS INTEGER NO-UNDO.
      
      liPeriod = YEAR(TODAY) * 100 + MONTH(TODAY).

      FOR
         EACH MobSub FIELDS (Brand MsSeq CustNum) NO-LOCK,
         EACH lbSingleFee NO-LOCK WHERE
            lbSingleFee.Brand       = MobSub.Brand         AND
            lbSingleFee.CustNum     = MobSub.CustNum       AND
            lbSingleFee.HostTable   = "MobSub"             AND
            lbSingleFee.KeyValue    = STRING(MobSub.MsSeq) AND
            lbSingleFee.BillPeriod >= liPeriod
         ON QUIT UNDO, RETRY
         ON STOP UNDO, RETRY:

         IF RETRY THEN DO:
            llInterrupted = TRUE.
            LEAVE.
         END.

         IF lbSingleFee.Concerns[1] = 0 OR lbSingleFee.Concerns[1] = ?
         THEN NEXT.

         liEvents = liEvents + 1.

         objDumpFile:mWriteALine().

         IF NOT SESSION:BATCH AND liEvents MOD 100 = 0 THEN 
         DO:
            PAUSE 0.
            DISPLAY liEvents LABEL "SingleFee Counter" 
               WITH OVERLAY ROW 10 CENTERED SIDE-LABELS
               TITLE " Collecting " FRAME fQty.
         END.      
         
      END.

      IF NOT SESSION:BATCH THEN
         HIDE FRAME fQty NO-PAUSE.

   END METHOD.

END CLASS.