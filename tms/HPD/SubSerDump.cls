USING Progress.Lang.*.
USING HPD.DumpHandler.
USING HPD.DumpBase.
USING HPD.DumpFile.

{Syst/tmsconst.i}

ROUTINE-LEVEL ON ERROR UNDO, THROW.

CLASS HPD.SubSerDump FINAL INHERITS DumpHandler:

   DEFINE PRIVATE BUFFER lbSubSer FOR SubSer.
  
   CONSTRUCTOR PUBLIC SubSerDump
      (iiDumpID AS INTEGER):
               
      objDumpBase = NEW DumpBase(iiDumpID, THIS-OBJECT).      
      objDumpBase:mUseTable(BUFFER lbSubSer:HANDLE).
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


   METHOD OVERRIDE PUBLIC CHARACTER mProcessRepLog
      (INPUT icRowID AS CHARACTER):

      FIND lbSubSer NO-LOCK WHERE
         ROWID(lbSubSer) = TO-ROWID(icRowID)
      NO-ERROR.

      IF AVAILABLE lbSubSer
      THEN DO:
         FIND CURRENT lbSubSer NO-LOCK.

         FOR
            FIRST MobSub FIELDS (MsSeq) NO-LOCK WHERE
               MobSub.MsSeq = lbSubSer.MsSeq:
            RETURN objDumpBase:mGetData().
         END.
      END.

      RETURN "".

   END METHOD.


   METHOD PRIVATE VOID mProcessFullDump
      (INPUT objDumpFile AS CLASS DumpFile):

      DEFINE VARIABLE lcServComs   AS CHARACTER NO-UNDO.
      DEFINE VARIABLE lcServCom    AS CHARACTER NO-UNDO.
      DEFINE VARIABLE liNumEntries AS INTEGER   NO-UNDO.
      DEFINE VARIABLE liCount      AS INTEGER   NO-UNDO.

      ASSIGN
         lcServComs = {&HPD_SERVICES}
         liNumEntries = NUM-ENTRIES(lcServComs).

      FOR
         EACH MobSub FIELDS (MsSeq) NO-LOCK:

         DO liCount = 1 TO liNumEntries:

            lcServCom = ENTRY(liCount,lcServComs).

            FOR
               FIRST lbSubSer NO-LOCK WHERE
                  lbSubSer.MsSeq   = MobSub.MsSeq AND
                  lbSubSer.ServCom = lcServCom
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
                  DISPLAY liEvents LABEL "SubSer Counter"
                     WITH OVERLAY ROW 10 CENTERED SIDE-LABELS
                     TITLE " Collecting " FRAME fQty.
               END.
            END.
         END.

      END.
      
      IF NOT SESSION:BATCH THEN 
         HIDE FRAME fQty NO-PAUSE.    

   END METHOD.

END CLASS.