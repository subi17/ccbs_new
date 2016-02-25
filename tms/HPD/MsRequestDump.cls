USING Progress.Lang.*.
USING HPD.DumpHandler.
USING HPD.DumpBase.

{HPD/HPDConst.i}
{Syst/tmsconst.i}

ROUTINE-LEVEL ON ERROR UNDO, THROW.

CLASS HPD.MsRequestDump FINAL INHERITS DumpHandler:

   DEFINE PRIVATE BUFFER lbMsRequest     FOR MsRequest.
  
   CONSTRUCTOR PUBLIC MsRequestDump
      (iiDumpID AS INTEGER):
      
      lhMainTable = BUFFER lbMsRequest:HANDLE.
         
      objDumpBase = NEW DumpBase(iiDumpID, THIS-OBJECT).
      objDumpBase:mUseTable(lhMainTable).
      objDumpBase:mVerifyDumpFileData().
      
   END CONSTRUCTOR.
   

   CONSTRUCTOR PUBLIC MsRequestDump
      (iiDumpID    AS INTEGER,
       icFile      AS CHARACTER):
               
      objDumpBase = NEW DumpTimeBased(iiDumpID, icFile, THIS-OBJECT).
      objDumpBase:mUseTable(BUFFER lbMsRequest:HANDLE).
      objDumpBase:mVerifyDumpFileData().

      mProcessTimeBased(INPUT CAST(objDumpBase, DumpTimeBased)).
      
      /* If there has been problems we won't be here... */ 
      IF llInterrupted = FALSE
      THEN llAllOK = TRUE.
      
   END CONSTRUCTOR.  


   METHOD PRIVATE CHARACTER mReqSource():
      
      IF lbMsRequest.ReqSource EQ ""
      THEN DO:         
         IF lbMsRequest.UserCode BEGINS "VISTA_"
         THEN RETURN {&REQUEST_SOURCE_NEWTON}.
         
         RETURN {&REQUEST_SOURCE_MANUAL_TMS}.
      END.
      
      RETURN lbMsRequest.ReqSource.
   
   END METHOD. 


   METHOD PUBLIC OVERRIDE CHARACTER mSpecialHandling
      (icField     AS CHARACTER,
       ilCheckOnly AS LOGICAL):

      CASE icField:
         WHEN "ReqSource"
         THEN IF ilCheckOnly
              THEN RETURN "OK".
              ELSE RETURN mReqSource().
         OTHERWISE RETURN "".         
      END CASE.

   END METHOD.


   METHOD PRIVATE VOID mProcessTimeBased
      (INPUT objDumpTB AS CLASS DumpTimeBased):

      DEFINE VARIABLE lcReqStatuses AS CHARACTER NO-UNDO.
      DEFINE VARIABLE liReqStatus   AS INTEGER   NO-UNDO.
      DEFINE VARIABLE lii           AS INTEGER   NO-UNDO.

      lcReqStatuses = {&REQ_INACTIVE_STATUSES} + "," + {&REQ_ONGOING_STATUSES}.

      DO lii = 1 TO NUM-ENTRIES(lcReqStatuses):
         
         liReqStatus = INTEGER(ENTRY(lii,lcReqStatuses)).
         
         FOR
            EACH lbMsRequest NO-LOCK WHERE
               lbMsRequest.Brand     = "1"                 AND
               lbMsRequest.ReqStatus = liReqStatus         AND
               lbMsRequest.ActStamp >= objDumpTB:ldeFromTS AND
               lbMsRequest.ActStamp  < objDumpTB:ldeToTS
            ON QUIT UNDO, RETRY
            ON STOP UNDO, RETRY:

            IF RETRY THEN DO:
               llInterrupted = TRUE.
               LEAVE.
            END.

            IF LOOKUP(STRING(lbMsRequest.ReqType),{&REQTYPES_HPD}) EQ 0
            THEN NEXT.
            
            IF lbMsRequest.ReqSource > "" AND
               LOOKUP(lbMsRequest.ReqSource,{&REQUEST_SOURCES_HPD}) = 0
            THEN NEXT.

            liEvents = liEvents + 1.

            objDumpTB:mWriteALine().
         
            IF NOT SESSION:BATCH AND liEvents MOD 100 = 0 THEN 
            DO:
               PAUSE 0.
               DISPLAY liEvents LABEL "MsRequest Counter" 
                  WITH OVERLAY ROW 10 CENTERED SIDE-LABELS
                  TITLE " Collecting " FRAME fQty.
            END.         
   
         END.                  
         
      END.
      
      IF NOT SESSION:BATCH THEN 
         HIDE FRAME fQty NO-PAUSE.      
         
   END METHOD.   

END CLASS.