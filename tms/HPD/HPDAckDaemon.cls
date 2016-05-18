USING Progress.Lang.*.

ROUTINE-LEVEL ON ERROR UNDO, THROW.

CLASS HPD.HPDAckDaemon INHERITS gearbox.daemons.Daemon:  
   
   DEFINE PRIVATE VARIABLE objHPDAckReader      AS CLASS HPD.HPDAckReader NO-UNDO.   
   DEFINE PRIVATE VARIABLE lcSingleReplogDBList AS CHARACTER              NO-UNDO.
   
   CONSTRUCTOR PUBLIC HPDAckDaemon( ):

      IF LOG-MANAGER:LOGGING-LEVEL < 2 AND LOG-MANAGER:LOGFILE-NAME <> ?
      THEN LOG-MANAGER:WRITE-MESSAGE("Daemon started", "INFO").

   END CONSTRUCTOR.     
     
     
   METHOD OVERRIDE PUBLIC VOID setUp ():

      DEFINE VARIABLE lcInstance AS CHARACTER NO-UNDO.
      ASSIGN
         lcInstance = REPLACE(lDaemonName, THIS-OBJECT:GetClass():TypeName, "")
         lcSingleReplogDBList = Syst.Parameters:getc("HPD.Ack.SingleReplogDB", "HPD.Interface")
         .   
      
      IF LOOKUP(lcInstance,lcSingleReplogDBList) > 0
      THEN objHPDAckReader = NEW HPD.HPDAckReaderSingle(lcInstance).
      ELSE objHPDAckReader = NEW HPD.HPDAckReaderMulti(lcInstance).

   END METHOD.
  

   METHOD OVERRIDE PUBLIC VOID shutDown ():
      
      IF VALID-OBJECT(objHPDAckReader)
      THEN DELETE OBJECT objHPDAckReader.
      
   END METHOD.
   
   
   DESTRUCTOR PUBLIC HPDAckDaemon( ):

      shutDown().

      IF LOG-MANAGER:LOGGING-LEVEL < 2 AND LOG-MANAGER:LOGFILE-NAME <> ?
      THEN LOG-MANAGER:WRITE-MESSAGE("Daemon stopped", "INFO").
                 
   END DESTRUCTOR.   


   METHOD PUBLIC OVERRIDE LOGICAL processOne():

      objHPDAckReader:mProcess().
      
      RETURN FALSE.
      
   END METHOD.

END CLASS.