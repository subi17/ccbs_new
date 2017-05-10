/*------------------------------------------------------------------------
    File        : MManDaemon
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : tlamminmaki
    Notes       : 
  ----------------------------------------------------------------------*/
USING Progress.Lang.*.

ROUTINE-LEVEL ON ERROR UNDO, THROW.

CLASS Mm.MManDaemon INHERITS gearbox.daemons.Daemon:  
   
   DEFINE PRIVATE VARIABLE objMManHandler AS CLASS Mm.MManHandler NO-UNDO.
   
   CONSTRUCTOR PUBLIC MManDaemon( ):

      IF LOG-MANAGER:LOGGING-LEVEL < 2 AND LOG-MANAGER:LOGFILE-NAME <> ?
      THEN LOG-MANAGER:WRITE-MESSAGE("Daemon started", "INFO").

   END CONSTRUCTOR.     


   METHOD OVERRIDE PUBLIC VOID setUp ():

      objMManHandler = NEW Mm.MManHandler().

   END METHOD.


   METHOD OVERRIDE PUBLIC VOID shutDown ():
      
      IF VALID-OBJECT(objMManHandler)
      THEN DELETE OBJECT objMManHandler.
      
   END METHOD.
   

   DESTRUCTOR PUBLIC MManDaemon( ):

      shutDown().

      IF LOG-MANAGER:LOGGING-LEVEL < 2 AND LOG-MANAGER:LOGFILE-NAME <> ?
      THEN LOG-MANAGER:WRITE-MESSAGE("Daemon stopped", "INFO").
                 
   END DESTRUCTOR.   


   METHOD PUBLIC OVERRIDE LOGICAL processOne():

      IF objMManHandler:mIsEmpty()
      THEN pauseUntilHeartbeat(5).
      ELSE objMManHandler:mProcess().

      RETURN FALSE.

   END METHOD.

END CLASS.