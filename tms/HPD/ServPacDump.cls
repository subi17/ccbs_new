USING Progress.Lang.*.
USING HPD.DumpHandler.
USING HPD.DumpBase.
USING HPD.DumpFile.
USING HPD.DumpMQ.
USING HPD.DumpSomeWhere.

{HPD/HPDConst.i}

ROUTINE-LEVEL ON ERROR UNDO, THROW.

CLASS HPD.ServPacDump FINAL INHERITS DumpHandler:
  
   DEFINE PRIVATE BUFFER lbServPac FOR ServPac.
  
   CONSTRUCTOR PUBLIC ServPacDump
      (iiDumpID AS INTEGER):

      lhMainTable = BUFFER lbServPac:HANDLE.
      
      objDumpBase = NEW DumpBase(iiDumpID, THIS-OBJECT).
      objDumpBase:mUseTable(lhMainTable).
      objDumpBase:mVerifyDumpFileData().
      
   END CONSTRUCTOR.


   CONSTRUCTOR PUBLIC ServPacDump
      (iiDumpID    AS INTEGER,
       icFile      AS CHARACTER):

      IF icFile = "{&FileIDForMQ}"
      THEN objDumpBase = NEW DumpMQ(iiDumpID, THIS-OBJECT).
      ELSE objDumpBase = NEW DumpFile(iiDumpID, icFile, THIS-OBJECT).

      objDumpBase:mUseTable(BUFFER lbServPac:HANDLE).
      objDumpBase:mVerifyDumpFileData().

      mProcessFullDump(INPUT CAST(objDumpBase, DumpSomeWhere)).
      
      /* If there has been problems we won't be here... */
      IF llInterrupted = FALSE
      THEN llAllOK = TRUE.
      
   END CONSTRUCTOR. 


   METHOD PRIVATE VOID mProcessFullDump
      (INPUT objDumpSomeWhere AS CLASS DumpSomeWhere):

      FOR
         EACH lbServPac NO-LOCK WHERE
            lbServPac.Brand = "1"
         ON QUIT UNDO, RETRY
         ON STOP UNDO, RETRY:

         IF RETRY THEN DO:
            llInterrupted = TRUE.
            LEAVE.
         END.

         liEvents = liEvents + 1.

         objDumpSomeWhere:mWriteALine().

         IF NOT SESSION:BATCH AND liEvents MOD 100 = 0 THEN 
         DO:
            PAUSE 0.
            DISPLAY liEvents LABEL "ServPac Counter" 
               WITH OVERLAY ROW 10 CENTERED SIDE-LABELS
               TITLE " Collecting " FRAME fQty.
         END.      
         
      END.

      IF NOT SESSION:BATCH THEN
         HIDE FRAME fQty NO-PAUSE.

   END METHOD.

END CLASS.