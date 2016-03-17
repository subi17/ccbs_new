USING Progress.Lang.*.
USING HPD.DumpHandler.
USING HPD.DumpBase.
USING HPD.DumpTimeBased.

{HPD/HPDConst.i}

ROUTINE-LEVEL ON ERROR UNDO, THROW.

CLASS HPD.PrepaidRequestDump FINAL INHERITS DumpHandler:

   DEFINE PRIVATE BUFFER lbPrepaidRequest     FOR PrepaidRequest.
  
   CONSTRUCTOR PUBLIC PrepaidRequestDump
      (iiDumpID AS INTEGER):
      
      lhMainTable = BUFFER lbPrepaidRequest:HANDLE.
         
      objDumpBase = NEW DumpBase(iiDumpID, THIS-OBJECT).
      objDumpBase:mUseTable(lhMainTable).
      objDumpBase:mVerifyDumpFileData().
      
   END CONSTRUCTOR.
   

   CONSTRUCTOR PUBLIC PrepaidRequestDump
      (iiDumpID    AS INTEGER,
       icFile      AS CHARACTER):
               
      objDumpBase = NEW DumpTimeBased(iiDumpID, icFile, THIS-OBJECT).
      objDumpBase:mUseTable(BUFFER lbPrepaidRequest:HANDLE).
      objDumpBase:mVerifyDumpFileData().

      mProcessTimeBased(INPUT CAST(objDumpBase, DumpTimeBased)).
      
      /* If there has been problems we won't be here... */ 
      IF llInterrupted = FALSE
      THEN llAllOK = TRUE.
      
   END CONSTRUCTOR.


   METHOD PUBLIC OVERRIDE CHARACTER mSpecialHandling
      (icField     AS CHARACTER,
       ilCheckOnly AS LOGICAL):

      CASE icField:
         WHEN "TopUpAmt"
         THEN IF ilCheckOnly
              THEN RETURN "OK".
              ELSE RETURN STRING(lbPrepaidRequest.TopUpAmt / 100).
         WHEN "VatAmt"
         THEN IF ilCheckOnly
              THEN RETURN "OK".
              ELSE RETURN STRING(lbPrepaidRequest.VatAmt / 100).
         OTHERWISE RETURN "".         
      END CASE.

   END METHOD.

   
   METHOD PRIVATE CHARACTER mPopulatePPStatuses():
      
      DEFINE VARIABLE lcReturnValue AS CHARACTER NO-UNDO.
      
      FOR
         EACH TMSCodes NO-LOCK WHERE
            TMSCodes.TableName = "Prepaidrequest" AND
            TMSCodes.FieldName = "PPStatus":
      
         lcReturnValue = lcReturnValue + (IF lcReturnValue = "" THEN "" ELSE ",") + TMSCodes.CodeValue.
               
      END.

      RETURN lcReturnValue.
      
   END METHOD.



   METHOD PRIVATE VOID mProcessTimeBased
      (INPUT objDumpTB AS CLASS DumpTimeBased):

      DEFINE VARIABLE lcReqStatuses AS CHARACTER NO-UNDO.
      DEFINE VARIABLE liReqStatus   AS INTEGER   NO-UNDO.
      DEFINE VARIABLE lii           AS INTEGER   NO-UNDO.

      lcReqStatuses = mPopulatePPStatuses().

      DO lii = 1 TO NUM-ENTRIES(lcReqStatuses):
         
         liReqStatus = INTEGER(ENTRY(lii,lcReqStatuses)).

         FOR
            EACH lbPrepaidRequest NO-LOCK WHERE
               lbPrepaidRequest.Brand      = "1"                 AND
               lbPrepaidRequest.PPStatus   = liReqStatus         AND
               lbPrepaidRequest.TsRequest >= objDumpTB:ldeFromTS AND
               lbPrepaidRequest.TsRequest  < objDumpTB:ldeToTS
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
               DISPLAY liEvents LABEL "PrepaidRequest Counter" 
                  WITH OVERLAY ROW 10 CENTERED SIDE-LABELS
                  TITLE " Collecting " FRAME fQty.
            END.         
   
         END.                  
         
      END.
      
      IF NOT SESSION:BATCH THEN 
         HIDE FRAME fQty NO-PAUSE.      
         
   END METHOD.   

END CLASS.