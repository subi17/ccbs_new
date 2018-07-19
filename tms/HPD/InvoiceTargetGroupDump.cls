USING Progress.Lang.*.
USING HPD.DumpHandler.
USING HPD.DumpBase.
USING HPD.DumpFile.
USING HPD.DumpMQ.
USING HPD.DumpSomeWhere.

{HPD/HPDConst.i}

ROUTINE-LEVEL ON ERROR UNDO, THROW.

CLASS HPD.InvoiceTargetGroupDump FINAL INHERITS DumpHandler:

   DEFINE PRIVATE BUFFER lbInvoiceTargetGroup FOR InvoiceTargetGroup.
  
   CONSTRUCTOR PUBLIC InvoiceTargetGroupDump
      (iiDumpID AS INTEGER):

      objDumpBase = NEW DumpBase(iiDumpID, THIS-OBJECT).
      objDumpBase:mUseTable(BUFFER lbInvoiceTargetGroup:HANDLE).
      objDumpBase:mVerifyDumpFileData().
      
   END CONSTRUCTOR.


   CONSTRUCTOR PUBLIC InvoiceTargetGroupDump
      (iiDumpID    AS INTEGER,
       icFile      AS CHARACTER):

      IF icFile = "{&FileIDForMQ}"
      THEN objDumpBase = NEW DumpMQ(iiDumpID, THIS-OBJECT).
      ELSE objDumpBase = NEW DumpFile(iiDumpID, icFile, THIS-OBJECT).

      objDumpBase:mUseTable(BUFFER lbInvoiceTargetGroup:HANDLE).
      objDumpBase:mVerifyDumpFileData().

      mProcessFullDump(INPUT CAST(objDumpBase, DumpSomeWhere)).
      
      /* If there has been problems we won't be here... */
      IF llInterrupted = FALSE
      THEN llAllOK = TRUE.
      
   END CONSTRUCTOR.


   METHOD OVERRIDE PUBLIC CHARACTER mProcessRepLog
      (INPUT icRowID AS CHARACTER):

      FIND lbInvoiceTargetGroup NO-LOCK WHERE
         ROWID(lbInvoiceTargetGroup) = TO-ROWID(icRowID)
      NO-ERROR.

      IF AVAILABLE lbInvoiceTargetGroup
      THEN DO:
         FIND CURRENT lbInvoiceTargetGroup NO-LOCK.
              
         RETURN objDumpBase:mGetData().
         
      END.
            
      RETURN "". 

   END METHOD.


   METHOD PRIVATE VOID mProcessFullDump
      (INPUT objDumpSomeWhere AS CLASS DumpSomeWhere):

      FOR EACH lbInvoiceTargetGroup NO-LOCK
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
            DISPLAY liEvents LABEL "InvoiceTargetGroup Counter"
               WITH OVERLAY ROW 10 CENTERED SIDE-LABELS
               TITLE " Collecting " FRAME fQty.
         END.      

      END.

      IF NOT SESSION:BATCH THEN
         HIDE FRAME fQty NO-PAUSE.

   END METHOD.

END CLASS.