USING Progress.Lang.*.
USING HPD.DumpHandler.
USING HPD.DumpBase.
USING HPD.DumpFile.

ROUTINE-LEVEL ON ERROR UNDO, THROW.

CLASS HPD.FATimeDump FINAL INHERITS DumpHandler:
  
   DEFINE PRIVATE BUFFER lbFATime FOR FATime.
  
   CONSTRUCTOR PUBLIC FATimeDump
      (iiDumpID AS INTEGER):

      lhMainTable = BUFFER lbFATime:HANDLE.
      
      objDumpBase = NEW DumpBase(iiDumpID, THIS-OBJECT).
      objDumpBase:mUseTable(lhMainTable).
      objDumpBase:mVerifyDumpFileData().
      
   END CONSTRUCTOR.


   CONSTRUCTOR PUBLIC FATimeDump
      (iiDumpID    AS INTEGER,
       icFile      AS CHARACTER):

      objDumpBase = NEW DumpFile(iiDumpID, icFile, THIS-OBJECT).
      objDumpBase:mUseTable(BUFFER lbFATime:HANDLE).
      objDumpBase:mVerifyDumpFileData().

      mProcessFullDump(INPUT CAST(objDumpBase, DumpFile)).
      
      /* If there has been problems we won't be here... */
      IF llInterrupted = FALSE
      THEN llAllOK = TRUE.
      
   END CONSTRUCTOR.


   METHOD PRIVATE LOGICAL mSkipMsSeq
      (iiMsSeq AS INTEGER):

      FOR FIRST MobSub FIELDS (MsSeq) NO-LOCK WHERE
         MobSub.MsSeq = iiMsSeq:
         RETURN FALSE.
      END.

      FOR
         FIRST TermMobSub FIELDS (MsSeq) NO-LOCK WHERE
            TermMobSub.MsSeq = iiMsSeq,
         FIRST MSOwner FIELDS (MsSeq TsBegin TsEnd) USE-INDEX MsSeq NO-LOCK WHERE
            MSOwner.MsSeq = TermMobSub.MsSeq:

         IF HPDTime:mMake2Date(MSOwner.TsEnd) > TODAY - 60
         THEN RETURN FALSE.
            
      END.

      RETURN TRUE.
   
   END METHOD.


   METHOD PRIVATE VOID mProcessFullDump
      (INPUT objDumpFile AS CLASS DumpFile):

      FOR
         EACH lbFATime NO-LOCK
         ON QUIT UNDO, RETRY
         ON STOP UNDO, RETRY:

         IF RETRY THEN DO:
            llInterrupted = TRUE.
            LEAVE.
         END.
         
         IF mSkipMsSeq(lbFATime.MsSeq)
         THEN NEXT.
                  
         liEvents = liEvents + 1.

         objDumpFile:mWriteALine().

         IF NOT SESSION:BATCH AND liEvents MOD 100 = 0 THEN 
         DO:
            PAUSE 0.
            DISPLAY liEvents LABEL "FATime Counter" 
               WITH OVERLAY ROW 10 CENTERED SIDE-LABELS
               TITLE " Collecting " FRAME fQty.
         END.      
         
      END.

      IF NOT SESSION:BATCH THEN
         HIDE FRAME fQty NO-PAUSE.

   END METHOD.

END CLASS.