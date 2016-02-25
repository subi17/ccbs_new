/*------------------------------------------------------------------------
    File        : IMSIDump
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : tlamminmaki
    Created     : Wed Oct 29 15:19:10 EET 2014
    Notes       : 
  ----------------------------------------------------------------------*/
USING Progress.Lang.*.
USING HPD.DumpHandler.
USING HPD.DumpTimeBased.
USING HPD.DumpBase.
USING HPD.HPDTime.

ROUTINE-LEVEL ON ERROR UNDO, THROW.

CLASS HPD.IMSIDump FINAL INHERITS DumpHandler:
  
   DEFINE PRIVATE BUFFER lbIMSI FOR IMSI.
   
   CONSTRUCTOR PUBLIC IMSIDump
      (iiDumpID AS INTEGER):
      
      lhMainTable = BUFFER lbIMSI:HANDLE.
         
      objDumpBase = NEW DumpBase(iiDumpID, THIS-OBJECT).
      objDumpBase:mUseTable(lhMainTable).
      objDumpBase:mVerifyDumpFileData().
      
   END CONSTRUCTOR.
   

   CONSTRUCTOR PUBLIC IMSIDump
      (iiDumpID    AS INTEGER,
       icFile      AS CHARACTER):
               
      objDumpBase = NEW DumpTimeBased(iiDumpID, icFile, THIS-OBJECT).
      objDumpBase:mUseTable(BUFFER lbIMSI:HANDLE).
      objDumpBase:mVerifyDumpFileData().

      mProcessTimeBased(INPUT CAST(objDumpBase, DumpTimeBased)).
      
      /* If there has been problems we won't be here... */ 
      IF llInterrupted = FALSE
      THEN llAllOK = TRUE.
      
   END CONSTRUCTOR.


   METHOD PRIVATE VOID mProcessTimeBased
      (INPUT objDumpTB AS CLASS DumpTimeBased):
         
      DEFINE VARIABLE ldaFromDate AS DATE NO-UNDO.
      DEFINE VARIABLE ldaToDate AS DATE NO-UNDO.

      ASSIGN
         ldaFromDate = HPDTime:mMake2Date(objDumpTB:ldeFromTS)
         ldaToDate   = HPDTime:mMake2Date(objDumpTB:ldeToTS)
         .         

      FOR EACH lbIMSI NO-LOCK USE-INDEX ImportDate WHERE
         lbIMSI.ImportDate  >= ldaFromDate          AND
         lbIMSI.ImportDate  <= ldaToDate            AND
         lbIMSI.ImportStamp >= objDumpTB:ldeFromTS  AND
         lbIMSI.ImportStamp <  objDumpTB:ldeToTS
         ON QUIT UNDO, RETRY
         ON STOP UNDO, RETRY:
   
         IF RETRY THEN DO:
            llInterrupted = TRUE.
            LEAVE.
         END.            
            
         liEvents = liEvents + 1.
      
         objDumpTB:mWriteALine().
         
         IF NOT SESSION:BATCH AND liEvents MOD 100 = 0 THEN 
         DO:
            PAUSE 0.
            DISPLAY liEvents LABEL "IMSI Counter" 
               WITH OVERLAY ROW 10 CENTERED SIDE-LABELS
               TITLE " Collecting " FRAME fQty.
         END.         
   
      END.
      
      IF NOT SESSION:BATCH THEN 
         HIDE FRAME fQty NO-PAUSE.    
         
   END METHOD.   

END CLASS.