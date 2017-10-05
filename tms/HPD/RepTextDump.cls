USING Progress.Lang.*.
USING HPD.DumpHandler.
USING HPD.DumpBase.
USING HPD.DumpFile.
USING HPD.DumpMQ.
USING HPD.DumpSomeWhere.

{HPD/HPDConst.i}

ROUTINE-LEVEL ON ERROR UNDO, THROW.

CLASS HPD.RepTextDump FINAL INHERITS DumpHandler:
  
   DEFINE PRIVATE BUFFER lbRepText FOR RepText.
  
   CONSTRUCTOR PUBLIC RepTextDump
      (iiDumpID AS INTEGER):

      lhMainTable = BUFFER lbRepText:HANDLE.
      
      objDumpBase = NEW DumpBase(iiDumpID, THIS-OBJECT).
      objDumpBase:mUseTable(lhMainTable).
      objDumpBase:mVerifyDumpFileData().
      
   END CONSTRUCTOR.


   CONSTRUCTOR PUBLIC RepTextDump
      (iiDumpID    AS INTEGER,
       icFile      AS CHARACTER):

      IF icFile = "{&FileIDForMQ}"
      THEN objDumpBase = NEW DumpMQ(iiDumpID, THIS-OBJECT).
      ELSE objDumpBase = NEW DumpFile(iiDumpID, icFile, THIS-OBJECT).

      objDumpBase:mUseTable(BUFFER lbRepText:HANDLE).
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
              ELSE RETURN {HPD/keyvalue.i lbRepText . objDumpBase:lcKeyDelimiter TextType LinkCode Language ToDate}.
         WHEN "RepText"
         THEN IF ilCheckOnly
              THEN RETURN "OK".
              ELSE RETURN IF lbRepText.RepText = "" THEN "" ELSE mRemoveLFCR(lbRepText.RepText).
         OTHERWISE RETURN "".         
      END CASE.

   END METHOD.


   METHOD PRIVATE CHARACTER mRemoveLFCR
      (icText AS CHARACTER):
         
      RETURN REPLACE(REPLACE(icText,CHR(10)," "),CHR(13)," ").
      
   END METHOD.


   METHOD PRIVATE VOID mProcessFullDump
      (INPUT objDumpSomeWhere AS CLASS DumpSomeWhere):

      DEFINE VARIABLE ldaToday AS DATE NO-UNDO.
      
      ldaToday = TODAY.   

      FOR
         EACH lbRepText NO-LOCK WHERE
            lbRepText.Brand   = "1"  AND
            lbRepText.ToDate >= ldaToday
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
            DISPLAY liEvents LABEL "RepText Counter" 
               WITH OVERLAY ROW 10 CENTERED SIDE-LABELS
               TITLE " Collecting " FRAME fQty.
         END.      
         
      END.

      IF NOT SESSION:BATCH THEN
         HIDE FRAME fQty NO-PAUSE.

   END METHOD.

END CLASS.