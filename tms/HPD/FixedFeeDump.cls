USING Progress.Lang.*.
USING HPD.DumpHandler.
USING HPD.DumpBase.
USING HPD.DumpFile.

ROUTINE-LEVEL ON ERROR UNDO, THROW.

CLASS HPD.FixedFeeDump FINAL INHERITS DumpHandler:

   DEFINE PRIVATE BUFFER lbFixedFee FOR FixedFee.
  
   CONSTRUCTOR PUBLIC FixedFeeDump
      (iiDumpID AS INTEGER):

      objDumpBase = NEW DumpBase(iiDumpID, THIS-OBJECT).
      objDumpBase:mUseTable(BUFFER lbFixedFee:HANDLE).
      objDumpBase:mVerifyDumpFileData().
      
   END CONSTRUCTOR.


   CONSTRUCTOR PUBLIC FixedFeeDump
      (iiDumpID    AS INTEGER,
       icFile      AS CHARACTER):

      objDumpBase = NEW DumpFile(iiDumpID, icFile, THIS-OBJECT).
      objDumpBase:mUseTable(BUFFER lbFixedFee:HANDLE).
      objDumpBase:mVerifyDumpFileData().

      mProcessFullDump(INPUT CAST(objDumpBase, DumpFile)).
      
      /* If there has been problems we won't be here... */
      IF llInterrupted = FALSE
      THEN llAllOK = TRUE.
      
   END CONSTRUCTOR.


   METHOD OVERRIDE PUBLIC CHARACTER mProcessRepLog
      (INPUT icRowID AS CHARACTER):

      FIND lbFixedFee NO-LOCK WHERE
         ROWID(lbFixedFee) = TO-ROWID(icRowID)
      NO-ERROR.

      IF AVAILABLE lbFixedFee
      THEN DO:
          IF lbFixedFee.HostTable NE "MobSub"
          THEN RETURN "".
          FOR
             FIRST MobSub FIELDS (MsSeq) NO-LOCK WHERE
                MobSub.MsSeq = INTEGER(lbFixedFee.KeyValue):
             RETURN objDumpBase:mGetData().
          END.
      END.

      RETURN "".

   END METHOD.


   METHOD PRIVATE VOID mProcessFullDump
      (INPUT objDumpFile AS CLASS DumpFile):

      DEFINE VARIABLE ldaDate  AS DATE    NO-UNDO.
      DEFINE VARIABLE liPeriod AS INTEGER NO-UNDO.

      ASSIGN
         ldaDate = ADD-INTERVAL(DATE(MONTH(TODAY),1,YEAR(TODAY)), -6, "months")
         liPeriod = YEAR(ldaDate) * 100 + MONTH(ldaDate)
         .

      FOR
         EACH MobSub FIELDS (Brand MsSeq CustNum) NO-LOCK,
         EACH lbFixedFee NO-LOCK WHERE
            lbFixedFee.Brand     = MobSub.Brand         AND
            lbFixedFee.CustNum   = MobSub.CustNum       AND
            lbFixedFee.HostTable = "MobSub"             AND
            lbFixedFee.KeyValue  = STRING(MobSub.MsSeq) AND
            lbFixedFee.EndPeriod >= liPeriod
         ON QUIT UNDO, RETRY
         ON STOP UNDO, RETRY:

         IF RETRY THEN DO:
            llInterrupted = TRUE.
            LEAVE.
         END.

         liEvents = liEvents + 1.

         objDumpFile:mWriteALine().

         IF NOT SESSION:BATCH AND liEvents MOD 100 = 0 THEN
         DO:
            PAUSE 0.
            DISPLAY liEvents LABEL "FixedFee Counter"
               WITH OVERLAY ROW 10 CENTERED SIDE-LABELS
               TITLE " Collecting " FRAME fQty.
         END.      

      END.

      IF NOT SESSION:BATCH THEN
         HIDE FRAME fQty NO-PAUSE.

   END METHOD.

END CLASS.