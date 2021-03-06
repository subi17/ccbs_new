USING Progress.Lang.*.
USING HPD.DumpHandler.
USING HPD.DumpBase.
USING HPD.DumpFile.
USING HPD.DumpMQ.
USING HPD.DumpSomeWhere.

{HPD/HPDConst.i}

ROUTINE-LEVEL ON ERROR UNDO, THROW.

CLASS HPD.DCCLIDump FINAL INHERITS DumpHandler:

   DEFINE PRIVATE BUFFER lbDCCLI FOR DCCLI.
  
   CONSTRUCTOR PUBLIC DCCLIDump
      (iiDumpID AS INTEGER):

      objDumpBase = NEW DumpBase(iiDumpID, THIS-OBJECT).
      objDumpBase:mUseTable(BUFFER lbDCCLI:HANDLE).
      objDumpBase:mVerifyDumpFileData().
      
   END CONSTRUCTOR.


   CONSTRUCTOR PUBLIC DCCLIDump
      (iiDumpID    AS INTEGER,
       icFile      AS CHARACTER):

      IF icFile = "{&FileIDForMQ}"
      THEN objDumpBase = NEW DumpMQ(iiDumpID, THIS-OBJECT).
      ELSE objDumpBase = NEW DumpFile(iiDumpID, icFile, THIS-OBJECT).

      objDumpBase:mUseTable(BUFFER lbDCCLI:HANDLE).
      objDumpBase:mVerifyDumpFileData().

      mProcessFullDump(INPUT CAST(objDumpBase, DumpSomeWhere)).
      
      /* If there has been problems we won't be here... */
      IF llInterrupted = FALSE
      THEN llAllOK = TRUE.
      
   END CONSTRUCTOR.


   METHOD OVERRIDE PUBLIC CHARACTER mProcessRepLog
      (INPUT icRowID AS CHARACTER):

      FIND lbDCCLI NO-LOCK WHERE
         ROWID(lbDCCLI) = TO-ROWID(icRowID)
      NO-ERROR.

      IF AVAILABLE lbDCCLI
      THEN DO:
         FIND CURRENT lbDCCLI NO-LOCK.
         FOR
            FIRST MobSub FIELDS (MsSeq) NO-LOCK WHERE
               MobSub.MsSeq = lbDCCLI.MsSeq:
            RETURN objDumpBase:mGetData().
         END.
      END.

      RETURN "".

   END METHOD.


   METHOD PRIVATE VOID mProcessFullDump
      (INPUT objDumpSomeWhere AS CLASS DumpSomeWhere):

      DEFINE VARIABLE ldaFromDate AS DATE NO-UNDO.

      ldaFromDate = ADD-INTERVAL(DATE(MONTH(TODAY),1,YEAR(TODAY)), -6, "months").

      FOR
         EACH MobSub FIELDS (MsSeq) NO-LOCK,
         EACH lbDCCLI NO-LOCK USE-INDEX MSSeq WHERE
            lbDCCLI.MsSeq   = MobSub.MsSeq AND
            lbDCCLI.ValidTo >= ldaFromDate
         ON QUIT UNDO, RETRY
         ON STOP UNDO, RETRY:

         liEvents = liEvents + 1.

         objDumpSomeWhere:mWriteALine().

         IF NOT SESSION:BATCH AND liEvents MOD 100 = 0 THEN 
         DO:
            PAUSE 0.
            DISPLAY liEvents LABEL "DCCLI Counter" 
               WITH OVERLAY ROW 10 CENTERED SIDE-LABELS
               TITLE " Collecting " FRAME fQty.
         END.

      END.

      IF NOT SESSION:BATCH THEN
         HIDE FRAME fQty NO-PAUSE.

   END METHOD.

END CLASS.