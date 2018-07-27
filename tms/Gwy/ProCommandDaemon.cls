/*------------------------------------------------------------------------
    File        : ProCommandDaemon
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : tlamminmaki
    Notes       : 
  ----------------------------------------------------------------------*/
USING Progress.Lang.*.

BLOCK-LEVEL ON ERROR UNDO, THROW.

CLASS Gwy.ProCommandDaemon INHERITS gearbox.daemons.Daemon:  
   
   DEFINE PRIVATE VARIABLE aoProCommandHandler AS CLASS Gwy.ProCommandHandler NO-UNDO.

   DEFINE PRIVATE VARIABLE ldttzNextReportTime AS DATETIME-TZ            NO-UNDO.
   DEFINE PRIVATE VARIABLE liQueue             AS INTEGER                NO-UNDO.
   DEFINE PRIVATE VARIABLE liInterval          AS INTEGER                NO-UNDO.
   DEFINE PRIVATE VARIABLE liPauseTime         AS INTEGER                NO-UNDO.
   DEFINE PRIVATE VARIABLE liPauseTotal        AS INTEGER                NO-UNDO.
   DEFINE PRIVATE VARIABLE liWaitTime          AS INTEGER                NO-UNDO.
   DEFINE PRIVATE VARIABLE llWait              AS LOGICAL INITIAL FALSE  NO-UNDO.
   
   CONSTRUCTOR PUBLIC ProCommandDaemon( ):

      IF multitenancy.TenantInformation:RealTenant NE multitenancy.TenantInformation:SuperTenant
      THEN RETURN ERROR NEW Progress.Lang.AppError
            ("This daemon must be run using the super tenant", 1). 

      lMaxConsecutiveErrors = 1.

      IF LOG-MANAGER:LOGGING-LEVEL < 2 AND LOG-MANAGER:LOGFILE-NAME <> ?
      THEN LOG-MANAGER:WRITE-MESSAGE("Daemon started", "INFO").

   END CONSTRUCTOR.     


   METHOD OVERRIDE PUBLIC VOID setUp ():

      DEFINE VARIABLE lcInstance AS CHARACTER NO-UNDO.
      
      lcInstance = REPLACE(lDaemonName, THIS-OBJECT:GetClass():TypeName, "").
      
      IF lcInstance BEGINS "NB_AS"
      THEN lcInstance = "NB_AS".
      ELSE IF lcInstance BEGINS "NB_CH"
      THEN lcInstance = "NB_CH".
      ELSE IF lcInstance BEGINS "NB"
      THEN lcInstance = "NB_AS,NB_CH".
      ELSE IF lcInstance BEGINS "BPM"
      THEN lcInstance = "BPM".
      ELSE IF lcInstance BEGINS "ALL"
      THEN lcInstance = "NB_AS,NB_CH,BPM".
      ELSE RETURN ERROR NEW Progress.Lang.AppError
            ("The daemon instance must begin with NB_AS, NB_CH, NB, BPM or ALL", 1).

      aoProCommandHandler = NEW Gwy.ProCommandHandler(lcInstance).

      ASSIGN
         liInterval       = 30 /* 1/2 minute */
         liWaitTime       = 0
         liPauseTotal     = 0
         llWait           = FALSE.

      mSetNewReportTime().

   END METHOD.


   METHOD OVERRIDE PUBLIC VOID shutDown ():
      
      IF VALID-OBJECT(aoProCommandHandler)
      THEN DELETE OBJECT aoProCommandHandler.
      
   END METHOD.
   

   METHOD PRIVATE VOID mSetNewReportTime():

      ldttzNextReportTime = ADD-INTERVAL(NOW, 5, "minutes").

   END METHOD.


   DESTRUCTOR PUBLIC ProCommandDaemon( ):

      shutDown().

      IF LOG-MANAGER:LOGGING-LEVEL < 2 AND LOG-MANAGER:LOGFILE-NAME <> ?
      THEN LOG-MANAGER:WRITE-MESSAGE("Daemon stopped", "INFO").
                 
   END DESTRUCTOR.   


   METHOD PUBLIC OVERRIDE LOGICAL processOne():

      IF NOW > ldttzNextReportTime
      THEN DO:
         aoProCommandHandler:mLogStatistic(ldttzNextReportTime).
         mSetNewReportTime().
      END.

      IF llWait
      THEN DO:
         ASSIGN
            liWaitTime   = MIN(liWaitTime + 1, liInterval)
            liPauseTime  = MIN(liWaitTime, 5)
            liPauseTotal = MIN(liPauseTotal + liPauseTime, liInterval).

         pauseUntilHeartbeat(liPauseTime).
      END.
      
      IF liPauseTotal >= liWaitTime 
      THEN DO:
         ASSIGN
            liPauseTotal = 0
            llWait = objReqHandler:mProcess().
         IF NOT llWait
         THEN liWaitTime = 0.
      END.

      RETURN FALSE.

   END METHOD.

END CLASS.