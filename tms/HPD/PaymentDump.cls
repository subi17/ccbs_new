/*------------------------------------------------------------------------
    File        : PaymentDump
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

CLASS HPD.PaymentDump FINAL INHERITS DumpHandler:
  
   DEFINE PRIVATE BUFFER lbPayment FOR Payment.

   
   CONSTRUCTOR PUBLIC PaymentDump
      (iiDumpID AS INTEGER):
      
      lhMainTable = BUFFER lbPayment:HANDLE.
         
      objDumpBase = NEW DumpBase(iiDumpID, THIS-OBJECT).
      objDumpBase:mUseTable(lhMainTable).
      objDumpBase:mVerifyDumpFileData().
      
   END CONSTRUCTOR.
   

   CONSTRUCTOR PUBLIC PaymentDump
      (iiDumpID    AS INTEGER,
       icFile      AS CHARACTER):
               
      objDumpBase = NEW DumpTimeBased(iiDumpID, icFile, THIS-OBJECT).
      objDumpBase:mUseTable(BUFFER lbPayment:HANDLE).
      objDumpBase:mVerifyDumpFileData().

      mProcessTimeBased(INPUT CAST(objDumpBase, DumpTimeBased)).
      
      /* If there has been problems we won't be here... */ 
      IF llInterrupted = FALSE
      THEN llAllOK = TRUE.
      
   END CONSTRUCTOR.
   

   METHOD PRIVATE VOID mProcessTimeBased
      (INPUT objDumpTB AS CLASS DumpTimeBased):

      DEFINE VARIABLE ldaAccDate  AS DATE      NO-UNDO.
      DEFINE VARIABLE ldaOldLimit AS DATE      NO-UNDO.
      DEFINE VARIABLE ldaFromDate AS DATE      NO-UNDO.
      DEFINE VARIABLE ldaToDate   AS DATE      NO-UNDO.
      DEFINE VARIABLE lii         AS INTEGER   NO-UNDO.
      DEFINE VARIABLE lcPaymTypes AS CHARACTER INITIAL "1,8" NO-UNDO.
      DEFINE VARIABLE liPaymType  AS INTEGER   NO-UNDO.

      ASSIGN
         ldaFromDate = HPDTime:mMake2Date(objDumpTB:ldeFromTS)
         ldaAccDate  = ldaFromDate - 4
         ldaOldLimit = DATE(1,6,2013)
         ldaToDate   = HPDTime:mMake2Date(objDumpTB:ldeToTS)
         .

      DO lii = 1 TO NUM-ENTRIES(lcPaymTypes):

         liPaymType = INTEGER(ENTRY(lii,lcPaymTypes)).
            
         FOR
            EACH lbPayment NO-LOCK USE-INDEX PaymType WHERE
               lbPayment.Brand     = "1"        AND
               lbPayment.PaymType  = liPaymType AND
               lbPayment.AccDate  <= ldaToDate
            ON QUIT UNDO, RETRY
            ON STOP UNDO, RETRY:         
               
            IF RETRY THEN DO:
               llInterrupted = TRUE.
               LEAVE.
            END.            
   
            IF lbPayment.AccDate < ldaAccDate
            THEN LEAVE.
   
            IF lbPayment.InvDate     <  06/01/2013          OR
               lbPayment.ImportDate  <  ldaFromDate         OR
               lbPayment.ImportDate  >  ldaToDate           OR
               lbPayment.ImportStamp <  objDumpTB:ldeFromTS OR
               lbPayment.ImportStamp >= objDumpTB:ldeToTS
            THEN NEXT.

            liEvents = liEvents + 1.
         
            objDumpTB:mWriteALine().
            
            IF NOT SESSION:BATCH AND liEvents MOD 100 = 0 THEN 
            DO:
               PAUSE 0.
               DISPLAY liEvents LABEL "Payment Counter" 
                  WITH OVERLAY ROW 10 CENTERED SIDE-LABELS
                  TITLE " Collecting " FRAME fQty.
            END.         
      
         END.
      END.
          
      IF NOT SESSION:BATCH THEN 
         HIDE FRAME fQty NO-PAUSE.    
         
   END METHOD.   

END CLASS.