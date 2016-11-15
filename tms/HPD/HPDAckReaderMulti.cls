USING Progress.Lang.*.

ROUTINE-LEVEL ON ERROR UNDO, THROW.

CLASS HPD.HPDAckReaderMulti INHERITS HPD.HPDAckReader:

   DEFINE PRIVATE TEMP-TABLE ttAckHandler NO-UNDO
      FIELD DBId          AS CHARACTER
      FIELD objHandler    AS CLASS Progress.Lang.Object /*HPD.HPDAckHandler*/
      INDEX DBId IS PRIMARY UNIQUE DBId
      .

   CONSTRUCTOR PUBLIC HPDAckReaderMulti
      ( icDB AS CHARACTER ):

      SUPER(icDB).

   END CONSTRUCTOR.


   DESTRUCTOR PUBLIC HPDAckReaderMulti():

      mDoAckReplog().

      FOR EACH ttAckHandler:

         IF VALID-OBJECT(ttAckHandler.objHandler)
         THEN DELETE OBJECT ttAckHandler.objHandler.

      END.

      EMPTY TEMP-TABLE ttAckHandler.

   END DESTRUCTOR.


   METHOD PRIVATE HPD.HPDAckHandler mGetHandler
      ( icDBId AS CHARACTER ):
   
      FOR ttAckHandler WHERE ttAckHandler.DBId = icDBId:
         
         RETURN CAST(ttAckHandler.objHandler, HPD.HPDAckHandler).
      
      END.
   
      CREATE ttAckHandler.
      
      ASSIGN
         ttAckHandler.objHandler = NEW HPD.HPDAckHandler(icDBId)
         ttAckHandler.DBId       = icDBId
         .
         
      RETURN CAST(ttAckHandler.objHandler, HPD.HPDAckHandler). 
   
   END.      

   
   METHOD PROTECTED OVERRIDE VOID mAckReplog():

      DEFINE VARIABLE loHPDAckHandler AS HPD.HPDAckHandler NO-UNDO.
      DEFINE VARIABLE liLoopCount     AS INTEGER           NO-UNDO.
      
      DO WHILE TRUE TRANSACTION ON ERROR UNDO, THROW:

         FOR
            EACH ttResponse
            BREAK
               BY ttResponse.DBId:
   
            IF FIRST-OF(ttResponse.DBId)
            THEN loHPDAckHandler = mGetHandler(ttResponse.DBId).
   
            IF loHPDAckHandler:mDelete(ttResponse.KeyValue,
                                       ttResponse.EventTime,
                                       ttResponse.ErrorCode,
                                       ttResponse.ErrorText)
            THEN DO:
               DELETE ttResponse.
               liAckCount = liAckCount - 1.
            END.
   
         END.
         
         liLoopCount = liLoopCount + 1.

         IF liAckCount <= 0 OR liLoopCount > liMaxLockedLoopCount
         THEN LEAVE.

         PAUSE 1 NO-MESSAGE.

      END.

   END METHOD.

   
   METHOD PROTECTED OVERRIDE VOID mCheckReplog():

      mDoAckReplog().
      
      FOR EACH ttAckHandler:
               
         CAST(ttAckHandler.objHandler, HPD.HPDAckHandler):mProcessWaitingReplog().
      
      END.

   END METHOD.   

END CLASS.