/*------------------------------------------------------------------------
    File        : HPDOnline
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : tlamminmaki
    Created     : Wed Oct 29 15:19:10 EET 2014
    Notes       : 
  ----------------------------------------------------------------------*/
USING Progress.Lang.*.

ROUTINE-LEVEL ON ERROR UNDO, THROW.

CLASS HPD.HPDOnline INHERITS gearbox.daemons.Daemon:  
   
   DEFINE PROTECTED VARIABLE objRepLogHandler    AS CLASS HPD.RepLogHandler  NO-UNDO.
   DEFINE PROTECTED VARIABLE lcInstance          AS CHARACTER                NO-UNDO.
   DEFINE PROTECTED VARIABLE ldaLastCheckingDate AS DATE                     NO-UNDO.
   
   CONSTRUCTOR PUBLIC HPDOnline( ):

      ldaLastCheckingDate = TODAY.

      IF LOG-MANAGER:LOGGING-LEVEL < 2 AND LOG-MANAGER:LOGFILE-NAME <> ?
      THEN LOG-MANAGER:WRITE-MESSAGE("Daemon started", "INFO").

   END CONSTRUCTOR.     


   METHOD OVERRIDE PUBLIC VOID setUp ():

      lcInstance = REPLACE(lDaemonName, THIS-OBJECT:GetClass():TypeName, "").
      objRepLogHandler = NEW HPD.RepLogHandler(lcInstance).

   END METHOD.


   METHOD OVERRIDE PUBLIC VOID shutDown ():
      
      IF VALID-OBJECT(objRepLogHandler)
      THEN DELETE OBJECT objRepLogHandler.
      
   END METHOD.
   

   METHOD PROTECTED VOID mReload():

      objRepLogHandler:mReload().

      ldaLastCheckingDate = TODAY.

   END METHOD.


   DESTRUCTOR PUBLIC HPDOnline( ):

      shutDown().

      IF LOG-MANAGER:LOGGING-LEVEL < 2 AND LOG-MANAGER:LOGFILE-NAME <> ?
      THEN LOG-MANAGER:WRITE-MESSAGE("Daemon stopped", "INFO").
                 
   END DESTRUCTOR.   


   METHOD PUBLIC OVERRIDE LOGICAL processOne():

      IF ldaLastCheckingDate NE TODAY
      THEN mReload().

      IF objRepLogHandler:mIsEmpty()
      THEN pauseUntilHeartbeat(5).
      ELSE objRepLogHandler:mProcess().

      RETURN FALSE.

   END METHOD.

END CLASS.