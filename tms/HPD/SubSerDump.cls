USING Progress.Lang.*.
USING HPD.DumpHandler.
USING HPD.DumpBase.
USING HPD.DumpFile.

{Syst/tmsconst.i}

ROUTINE-LEVEL ON ERROR UNDO, THROW.

CLASS HPD.SubSerDump FINAL INHERITS DumpHandler:

   DEFINE PRIVATE BUFFER lbSubSer     FOR SubSer.
  
   CONSTRUCTOR PUBLIC SubSerDump
      (iiDumpID AS INTEGER):
               
      lhMainTable = BUFFER lbSubSer:HANDLE.
      
      objDumpBase = NEW DumpBase(iiDumpID, THIS-OBJECT).      
      objDumpBase:mUseTable(lhMainTable).
      objDumpBase:mVerifyDumpFileData().
      
   END CONSTRUCTOR.


   CONSTRUCTOR PUBLIC SubSerDump
      (iiDumpID    AS INTEGER,
       icFile      AS CHARACTER):
               
      objDumpBase = NEW DumpFile(iiDumpID, icFile, THIS-OBJECT).
      objDumpBase:mUseTable(BUFFER lbSubSer:HANDLE).
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
              ELSE RETURN {HPD/keyvalue.i lbSubSer . objDumpBase:lcKeyDelimiter MsSeq ServCom SSDate}.
         OTHERWISE RETURN "".         
      END CASE.

   END METHOD.


   METHOD PRIVATE VOID mProcessFullDump
      (INPUT objDumpFile AS CLASS DumpFile):
   
      FOR EACH lbSubSer NO-LOCK
         ON QUIT UNDO, RETRY
         ON STOP UNDO, RETRY:
   
         IF RETRY THEN DO:
            llInterrupted = TRUE.
            LEAVE.
         END.            

         IF LOOKUP(lbSubSer.ServCom,{&HPD_SERVICES}) = 0
         THEN NEXT.
            
         liEvents = liEvents + 1.

         objDumpFile:mWriteALine().
         
         IF NOT SESSION:BATCH AND liEvents MOD 100 = 0 THEN 
         DO:
            PAUSE 0.
            DISPLAY liEvents LABEL "SubSer Counter" 
               WITH OVERLAY ROW 10 CENTERED SIDE-LABELS
               TITLE " Collecting " FRAME fQty.
         END.         
   
      END.
      
      IF NOT SESSION:BATCH THEN 
         HIDE FRAME fQty NO-PAUSE.    

   END METHOD.

END CLASS.