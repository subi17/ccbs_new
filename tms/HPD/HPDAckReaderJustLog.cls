USING Progress.Lang.*.

ROUTINE-LEVEL ON ERROR UNDO, THROW.

CLASS HPD.HPDAckReaderJustLog INHERITS HPD.HPDAckReader:

   DEFINE PRIVATE STREAM ackerrout.

   CONSTRUCTOR PUBLIC HPDAckReaderJustLog (icDB AS CHARACTER):

      SUPER(icDB).

   END CONSTRUCTOR.


   DESTRUCTOR PUBLIC HPDAckReaderJustLog():

      mAckReplog().
      
   END DESTRUCTOR.   


   METHOD PRIVATE VOID mLogErrorAck():

      FOR
         EACH ttResponse
         BREAK
            BY ttResponse.DBId:
            
         IF FIRST-OF(ttResponse.DBId)
         THEN OUTPUT STREAM ackerrout TO
               VALUE(lcExportDir + "/error_" + ttResponse.DBId + "_" +
               STRING(DAY(TODAY),"99") + STRING(MONTH(TODAY),"99") +
               STRING(YEAR(TODAY),"9999") + ".log") APPEND.
      
         PUT STREAM ackerrout UNFORMATTED
            ttResponse.KeyValue + " "  + 
            ISO-DATE(ttResponse.EventTime) + " " +
            STRING(ttResponse.ErrorCode) + " " +
            ttResponse.ErrorText SKIP.
      
         IF LAST-OF(ttResponse.DBId)
         THEN OUTPUT STREAM ackerrout CLOSE.
      
      END.
      
      FINALLY:
      
         OUTPUT STREAM ackerrout CLOSE.
         
      END FINALLY.
      
   END METHOD.


   METHOD PROTECTED OVERRIDE VOID mAckReplog():

      IF liAckCount > 0 OR
         CAN-FIND(FIRST ttResponse)
      THEN mLogErrorAck().
      
      FINALLY:
         ASSIGN
            liLastAck  = TIME
            liAckCount = 0
            .
         DATASET ldsAckDS:EMPTY-DATASET().   
      END FINALLY.
      
   END METHOD.
   

   METHOD PROTECTED OVERRIDE VOID mCheckReplog():

      ldtzNextCheckTime = ?.
      
   END METHOD.  
    
END CLASS.