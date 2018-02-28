ROUTINE-LEVEL ON ERROR UNDO, THROW.

CLASS Gwy.EventLogMaker:

   DEF VAR lhEventLog AS HANDLE NO-UNDO.
   DEF VAR lcUser     AS CHAR NO-UNDO.

   {Syst/eventval.i}
 
   CONSTRUCTOR PUBLIC EventLogMaker (icUser AS CHAR):
   
      RUN Syst/eventlog_super.p PERSISTENT SET lhEventLog.                
      lcUser = icUser.

   END CONSTRUCTOR.
   
   METHOD PUBLIC LOGIC make_eventlog 
      (icAction AS CHAR , 
       ihBuffer AS HANDLE) :

      IF llDoEvent THEN DO :
         RUN StarEventInitialize IN lhEventLog (ihBuffer).
                                                        
         CASE icAction:
         WHEN "Create"    THEN RUN StarEventMakeCreateEvent IN lhEventLog (lcUser, ihBuffer ).
         WHEN "OldBuffer" THEN RUN StarEventSetOldBuffer    IN lhEventLog (ihBuffer ).
         WHEN "Modify"    THEN RUN StarEventMakeModifyEvent IN lhEventLog (lcUser, ihBuffer ).
         WHEN "Delete"    THEN RUN StarEventMakeDeleteEvent IN lhEventLog (lcUser, ihBuffer ).
         END CASE.
      END.
            
   END METHOD.
            
   DESTRUCTOR PUBLIC EventLogMaker():
      
      IF VALID-HANDLE(lhEventLog) THEN DO: 
         RUN pCleanEventObjects in lhEventLog.
         DELETE PROCEDURE lhEventLog.
      END.
        
   END DESTRUCTOR.

END CLASS.

