USING Progress.Lang.*.

ROUTINE-LEVEL ON ERROR UNDO, THROW.

CLASS HPD.HPDAckReaderSingle INHERITS HPD.HPDAckReader:

   DEFINE VARIABLE loHPDAckHandler AS HPD.HPDAckHandler NO-UNDO.

   CONSTRUCTOR PUBLIC HPDAckReaderSingle
      ( icDB AS CHARACTER ):

      SUPER(icDB).

      loHPDAckHandler = NEW HPD.HPDAckHandler(icDB).

   END CONSTRUCTOR.


   DESTRUCTOR PUBLIC HPDAckReaderSingle():

      mAckReplog().

      IF VALID-OBJECT(loHPDAckHandler)
      THEN DELETE OBJECT loHPDAckHandler.

   END DESTRUCTOR.

   
   METHOD PROTECTED OVERRIDE VOID mAckReplog():

      DEFINE VARIABLE liLoopCount AS INTEGER NO-UNDO.
      
      DO WHILE TRUE TRANSACTION ON ERROR UNDO, THROW:

         FOR EACH ttResponse:
            
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
      
      SUPER:mAckReplog().
      
   END METHOD.

   
   METHOD PROTECTED OVERRIDE VOID mCheckReplog():

      mAckReplog().

      loHPDAckHandler:mProcessWaitingReplog().
      
      SUPER:mCheckReplog().

   END METHOD.

END CLASS.